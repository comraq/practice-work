{-# LANGUAGE FlexibleContexts
           , GeneralizedNewtypeDeriving
           , PatternGuards
  #-}

import Utils

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (catch, finally, SomeException)
import Control.Monad (replicateM_)
import Control.Monad.State
import Control.Monad.Except
import Data.Char (isControl)
import Data.List (nub)
import Network.URI
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as S
import Network.HTTP hiding (Done)
import Network.Stream (Result)

{-
 - In addition to the basic 'TVar' type, the 'stm' package also provides two
 - types that are more useful for communicating between threads.
 -
 - The 'TMVar' is the STM equivalent of an 'MVar' as it can hold either
 - 'Just a' or 'Nothing'.
 -
 - The 'TChan' type is the STM equivalent of 'Chan', and implements a typed
 - FIFO channel.
 -}

{-
 - A practical example/application of 'STM' is checking an HTML file for
 - broken/dead links. This problem suits being solved concurrently as we do
 - not want to block while waiting for dead servers to respond. By using
 - multiple threads, we can still get useful work done while one or two
 - requests are stuck at talking to slow or dead server.
 -
 - However, we also cannot simply create one thread per URL, as that may
 - overburden either the CPU or the network connection if most of the links
 - are live and responsive. Instead we will use a fixed number of worker
 - threads, which fetch URLs to download from a queue.
 -}

type URL = B.ByteString

data Task = Check URL | Done

main :: IO ()
main = do
    (files, k) <- parseArgs
    let n = length files

    -- A counter for broken links
    badCount <- newTVarIO (0 :: Int)

    -- A Channel to report broken links
    badLinks <- newTChanIO

    -- A Channel of 'Task's for sending jobs to workers
    jobs <- newTChanIO

    -- A counter for number of workers currently running
    workers <- newTVarIO k

    -- A thread that report bad links to stdout
    forkIO $ writeBadLinks badLinks

    -- Start worker threads
    forkTimes k workers (worker badLinks jobs badCount)

    {-
     - Read links from files, and enqueue them as jobs.
     - - checks the file for urls, construct a job for each
     - - start each job with empty job 'Set', 0 links found and the channel
     -   of 'Tasks'
     -}
    stats <- execJob (mapM_ checkURLs files)
                     (JobState S.empty 0 jobs)

    -- Enqueue the last 'Task' for each channel with 'Done'
    atomically $ replicateM_ k (writeTChan jobs Done)

    -- Wait for workers to finish
    waitFor workers

    broken <- atomically $ readTVar badCount
    printf fmt broken
               (linksFound stats)
               (S.size $ linksSeen stats)
               n

  where fmt = "Found %d broken links. " ++
              "Checked %d links (%d unique) in %d files.\n"

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ = modifyTVar

{-
 - 'forkTimes' starts a number of identical worker threads, and decreases
 - the "numAlive" count each time a thread exits. 'finally' ensures that the
 - decrement "numAlive" action is executed regardless of how each thread
 - forked is terminated.
 -}
forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k numAlive act = replicateM_ k . forkIO $
  act `finally` (atomically $ modifyTVar_ numAlive $ subtract 1)

-- Prints each broken or dead link to stdout
writeBadLinks :: TChan String -> IO ()
writeBadLinks c = forever $
  atomically (readTChan c) >>= putStrLn >> hFlush stdout

{-
 - 'check' is a function that calls 'retry' on an 'STM' action if the
 - boolean argument evaluates to 'False'
 -
 - Thus, 'waitFor' will end up blocking the current thread until 'numAlive'
 - evaluates to 0.
 -}
waitFor :: TVar Int -> IO ()
waitFor numAlive = atomically $ do
  count <- readTVar numAlive
  check (count == 0)

getStatus :: URI -> IO (Either String Int)
getStatus = chase (5 :: Int) -- Follow up to a max of 5 redirects
  where
    chase :: Int -> URI -> IO (Either String Int)
    chase 0 _ = bail "too many redirects"
    chase n u = do
      resp <- getHead u
      case resp of
        Left err -> bail $ show err
        Right r  -> case rspCode r of
          (3, _, _) -> case findHeader HdrLocation r of
            Nothing -> bail $ show r
            Just u' -> case parseURI u' of
              Nothing  -> bail "bad URL"
              Just url -> chase (n - 1) url

          (a, b, c) -> return . Right $ a * 100 + b * 10 + c

    bail = return . Left

getHead :: URI -> IO (Result (Response String))
getHead uri = simpleHTTP Request { rqURI     = uri
                                 , rqMethod  = HEAD
                                 , rqHeaders = []
                                 , rqBody    = "" }

{-
 - Using the 'ExceptT' monad transformer to refactor the regular 'getStatus'
 - function.
 -}
getStatusE :: URI -> IO (Either String Int)
getStatusE = runExceptT . chase (5 :: Int)
  where
    chase :: Int -> URI -> ExceptT String IO Int
    chase 0 _ = throwError "too many redirects"
    chase n u = do
      r <- embedEither show =<< (liftIO $ getHead u)
      case rspCode r of
        (3, _, _) -> do
          u'  <- embedMaybe (show r) $ findHeader HdrLocation r
          url <- embedMaybe "bad URL" $ parseURI u'
          chase (n - 1) url

        (a, b, c) -> return $ a * 100 + b * 10 + c

{-
 - Some handy embedding functions.
 -
 - 'embedEither' constructs a 'MonadError' instance from an 'Either' monad
 - instance if the value is 'Left'
 -
 - 'embedMaybe' constructs a 'MonadError' instance from a 'Maybe monad
 - instance if the value is 'Nothing'
 -
 -}
embedEither :: MonadError e m => (s -> e) -> Either s a -> m a
embedEither f = either (throwError . f) return

embedMaybe :: MonadError e m => e -> Maybe a -> m a
embedMaybe err = maybe (throwError err) return

{-
 - Each worker thread reads a task off the shared queue. It checks
 - the given URL:
 -   - if alive, return ()
 -   - if bad response code, encountered exception or invalid url,
 -     record in bad links channel, and increment bad links counter
 -
 - If no more URLs remain (ie: jobQueue is 'Done'), then exits
 -}
worker :: TChan String -> TChan Task -> TVar Int -> IO ()
worker badLinks jobQueue badCount = loop
  where
    -- Consume jobs until we are told to exit (ie: reach 'Done')
    loop :: IO ()
    loop = do
      job <- atomically $ readTChan jobQueue
      case job of
        Done    -> return ()
        Check x -> checkOne (B.unpack x) >> loop

    exceptionToLeft :: SomeException -> IO (Either String Int)
    exceptionToLeft = return . Left . show

    checkOne :: String -> IO ()
    checkOne url = case parseURI url of
      Just uri -> do
        code <- getStatusE uri `catch` exceptionToLeft
        case code of
          Right 200 -> return ()
          Right n   -> report $ show n
          Left  err -> report err

      _        -> report "invalid URL"

      where report :: String -> IO ()
            report s = atomically $ do
              modifyTVar_ badCount (+1)
              writeTChan badLinks $ url ++ " " ++ s

data JobState = JobState { linksSeen  :: S.Set URL
                         , linksFound :: Int
                         , linkQueue  :: TChan Task
                         }

newtype Job a = Job { runJob :: StateT JobState IO a }
  deriving (Functor, Applicative, Monad, MonadState JobState, MonadIO)

-- Executes a job and run the state by passing in the given job state
execJob :: Job a -> JobState -> IO JobState
execJob = execStateT . runJob

{-
 - Since the 'main' function is responsible for mapping 'checkURLs' over
 - each HTML file, 'checkURLs' only needs to be defined to read a single file
 -
 - - Gets all the urls from a file
 - - filters out all the ones already seen
 - - 'send' the jobs by writing them to the 'linkQueue' in the 'JobState'
 - - updates the 'linksFound' in the 'JobState'
 -}
checkURLs :: FilePath -> Job ()
checkURLs filepath = do
  src <- liftIO $ B.readFile filepath
  let urls = extractLinks src
  filterM seenURI urls >>= sendJobs
  updateStats $ length urls

-- Updates the 'linksFound' in 'JobState'
updateStats :: Int -> Job ()
updateStats a = modify $ \s -> s { linksFound = linksFound s + a }

-- | Add a link to the 'linksSeen' 'Set' we have seen
insertURI :: URL -> Job ()
insertURI c = modify $ \s -> s { linksSeen = S.insert c (linksSeen s) }

{- | If we have seen a link, return False. Otherwise, record that we have
seen it, and return True. -}
seenURI :: URL -> Job Bool
seenURI url = do
  seen <- not . S.member url <$> gets linksSeen
  insertURI url
  return seen

{-
 - "Sends jobs" by writing the urls to check as a task in the 'linkQueue' of
 - the 'JobState'
 -}
sendJobs :: [URL] -> Job ()
sendJobs jobs = do
  c <- gets linkQueue
  liftIO . atomically $ mapM_ (writeTChan c . Check) jobs

{-
 - Only looks for strings that appear to be URLs, not a full spec-ful URL
 - parser
 -}
extractLinks :: B.ByteString -> [URL]
extractLinks = concatMap uris . B.lines
  where uris s      = filter looksOkay $ B.splitWith isDelim s
        isDelim c   = isControl c || c `elem` " <>\"{}|\\^[]`"
        looksOkay s = http `B.isPrefixOf` s
        http        = B.pack "http:"


-- For parsing command line arguments

data Flag = Help | N Int
  deriving (Eq)

-- Note that pattern guards are used in this function
parseArgs :: IO ([String], Int)
parseArgs = do
    args <- getArgs
    case parse args of
      ([],   files,   [])               -> return (nub files, 16)
      (opts, files,   [])
        | Help `elem` opts              -> help
        | [N n] <- filter (/=Help) opts -> return (nub files, n)
      (_,    _,     errs)               -> die errs

  where
    {-
     - The 'getOpt' function takes three arguments:
     -   getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
     -
     - * An argument ordering, which specifies whether options can be mixed
     -   with other arguments or must appear before them.
     -   Note: The 'Permute' argument ordering used indicates that the arguments
     -         can be in any order
     -
     - * A list of option definitions. Each consists of a list of short
     -   names for the option, a list of long names for the option, a
     -   description of the option (ex: whether it accepts an argument), and
     -   an explanation for users. OptDescr is defined as follows:
     -
     -     data OptDescr a =
     -       Option [Char]         -- list of short option characters
     -              [String]       -- list of long option strings (without "--")
     -              (ArgDescr a)   -- argument descriptor
     -              String         -- explanation of option for user
     -
     - * A list of arguments and options, as returned by 'getArgs'
     -
     - The return type of 'getOpt' returns a triple which consists of the
     - parsed options, the remaining arguments, and any error messages that
     - arose.
     -}
    parse argv = getOpt Permute options argv
    header     = "Usage: urlcheck [-h] [-n n] [file ...]"
    info       = usageInfo header options
    dump       = hPutStrLn stderr
    die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help       = dump info                  >> exitWith ExitSuccess

{-
 - The 'Flag' ADT is used to represent options that the program can accept
 -
 - Note that the 'ArgDescr' type is defined as follows:
 -   data ArgDescr a =
 -       NoArg                   a         -- no argument expected
 -     | ReqArg (String ->       a) String -- an option that requires argument
 -     | OptArg (Maybe String -> a) String -- an optional argument
 -
 - * The 'NoArg' constructor accepts a parameter that will represent this
 -   option. In this case, if a user invokes this program with "-h" or
 -   "--help", we will use the value 'Help'.
 -
 - * The 'ReqArg' contructor accepts a function that maps a required
 -   argument to a value. Its second argument is used when printing help.
 -   Here, we convert a string into an integer, and pass it to the 'Flag'
 -   type's 'N' constructor
 -
 - * The 'OptArg' constructor is similar to the 'ReqArg' constructor, but it
 -   permits the use of options that can b used without arguments
 -}
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Show this help message",
            Option ['n'] []       (ReqArg (\s -> N $ read s) "N")
                   "Number of concurrent connections (default 16)" ]
