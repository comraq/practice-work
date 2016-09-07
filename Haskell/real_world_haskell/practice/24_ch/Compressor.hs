{-
 - Concurrency vs Parallelism:
 -
 - By definition, a concurrent program deals continuously with networking
 - protocols, databases and the like. A typical parallel porogram is likely
 - to be more focused: it streams data in, crucnches it for a while (with a
 - litter further I/O), then stream data back out.
 -
 - Quote from Simon Peyton Jones in "Tackling the Awkward Squad":
 -
 - * A parallel functional program uses multiple processors to gain
 -   performance. For example, it may be faster to evaluate "e1 + e2" by
 -   evaluate "e1" and "e2" in parallel, and the add the results. Parallelism
 -   has no semantic impact at all: the meaning of a program is unchanged
 -   whether it is executed sequentially or in parallel. Furthermore, the
 -   results are deterministic; there is no possiblity that a parallel program
 -   will give one result in one run and a different result in a different run.
 -
 - * In contrast, a concurrent program has concurrency as part of its
 -   specification. The program must run concurrent threads, each of which can
 -   independently perfrom input/output. THe program may be run on many
 -   processors, or on one -- that is an implementation choice. The behaviour
 -   of the program is, necessarily and by design, non-deterministic. Hence,
 -   unlike parallelism, concurrency has a substantial semantic impact.
 -}

import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)

-- Provided by the 'zlib' package on http://hackage.haskell.org/
import Codec.Compression.GZip (compress)

main :: IO ()
main = do
    maybeLine <- runInputT defaultSettings $ getInputLine "Enter a file to compress> "
    case maybeLine of
      Nothing   -> return ()    -- user entered EOF
      Just ""   -> return ()    -- treat no name as "want to quit"
      Just name -> do
        handle (print :: SomeException -> IO ()) $ do
          content <- L.readFile name
          forkIO (compressFile name content)
          return ()
        main

  where compressFile path = L.writeFile (path ++ ".gz") . compress
