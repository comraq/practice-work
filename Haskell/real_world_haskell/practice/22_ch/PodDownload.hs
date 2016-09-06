module PodDownload where

import PodTypes
import PodDB
import PodParser

import Network.HTTP
import System.IO
import Database.HDBC
import Data.Maybe
import Network.URI

{-
 - Note that the 'HTTP' library used in this module does not read HTTP
 - result lazily and thus can result in large consumption of RAM when
 - downloading large files.
 -
 - Alternatives include 'http-conduit' or 'Wreq[0]'
 -}

{- | Download a URL:
(Left errorMessage) if encountered error
(Right doc) if success.
-}
downloadURL :: String -> IO (Either String String)
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
      Left err -> return . Left $ "Error connecting: " ++ show err
      Right r  -> case rspCode r of
        (2, _, _) -> return . Right $ rspBody r

        -- Encountered a Http Redirect
        (3, _, _) -> case findHeader HdrLocation r of
           Nothing  -> return . Left . show $ r
           Just url -> downloadURL url

        -- Unsuccessful status code
        _         -> return . Left . show $ r

  where request = Request { rqURI     = uri
                          , rqMethod  = GET
                          , rqHeaders = []
                          , rqBody    = ""
                          }
        uri = fromJust $ parseURI url

{- | Update the podcast in the database. -}
updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbh pc = do
    resp <- downloadURL $ castURL pc
    case resp of
      Left  err -> putStrLn err
      Right doc -> updateDB doc

  where updateDB doc = do
            mapM_ (addEpisode dbh) episodes

          where feed     = parse doc $ castURL pc
                episodes = map (item2ep pc) (items feed)

{- | Downloads an episode, returning a String representing the filename it
was placed into, or Nothing on error. -}
getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbh ep = do
    resp <- downloadURL $ epURL ep
    case resp of
      Left  err -> do putStrLn err
                      return Nothing
      Right doc -> do
        file <- openBinaryFile filename WriteMode
        hPutStr file doc
        hClose file
        updateEpisode dbh $ ep { epDone = True }
        commit dbh
        return $ Just filename

  -- Apply an extension based on the filetype
  where filename = "pod." ++ (show . castId . epCast $ ep) ++ "." ++
                   (show $ epId ep) ++ ".mp3"
