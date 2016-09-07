{-
 - Below are some differences between the below TCP and the other UDP client:
 - - TCP is a streaming protocol, we can send data using a 'Handle' rather
 -   than lower level socket operations
 - - No need to store destination address in 'SyslogHandle' record in TCP
 -   client since we can use 'connect' to establish a TCP connection
 - - Since TCP messages may be spread out amongs multiple network packets,
 -   we will use the newline character '\n' as the end of message marker
 -   (though now a single message cannot contain a newline character)
 -}

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes
import System.IO

data SyslogHandle = SyslogHandle { slHandle  :: Handle
                                 , slProgram :: String
                                 }

openlog :: HostName         -- ^ Remote hostname, or localhost
        -> String           -- ^ Port numer of name, default to 514
        -> String           -- ^ Name to log under
        -> IO SyslogHandle  -- ^ Handle to use for logging
openlog hostname port progname = do
  {-
   - Look up the hostname and port. Either raises an exception or returns a
   - nonempty list. First element in that list is supposed to be the best
   - option.
   -}
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  -- Establish a socket for communication
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  {-
   - Mark the socket for keep-alive handling since it may be idle for long
   - periods of time
   -}
  setSocketOption sock KeepAlive 1

  -- Connect to the server
  connect sock $ addrAddress serveraddr

  -- Make a 'Handle' out of the socket for convenience
  h <- socketToHandle sock WriteMode

  {-
   - We are going to set buffering to 'BlockBuffering' and then explicitly
   - call 'hFlush' after each message, so that messages get logged
   - immediately
   -}
  hSetBuffering h $ BlockBuffering Nothing

  -- Save the socket, program name and server address in a handle
  return $ SyslogHandle h progname

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg = do
  hPutStrLn (slHandle syslogh) sendmsg

  -- Make sure that we send data immediately
  hFlush $ slHandle syslogh

  where code    = makeCode fac pri
        sendmsg = "<" ++ show code ++ ">" ++ slProgram syslogh ++ ": " ++ msg

closelog :: SyslogHandle -> IO ()
closelog syslogh = hClose $ slHandle syslogh

{- | Convert a facility and a priority into a syslog code -}
makeCode :: Facility -> Priority -> Int
makeCode fac pri =
  let faccode = codeOfFac fac
      pricode = fromEnum pri
  in  (faccode `shiftL` 3) .|. pricode
