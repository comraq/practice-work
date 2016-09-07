{-
 - Traditional Unix syslog services allows programs to send og messages over
 - a network to a central server that records them and uses UDP as the
 - transport protocol.
 -}

import Data.List
import Control.Monad (unless)

import Data.Bits
import qualified Data.ByteString.Char8 as Strict
import Network.Socket hiding (sendTo)
import Network.Socket.ByteString (sendTo)
import Network.BSD

import SyslogTypes

data SyslogHandle = SyslogHandle { slSocket  :: Socket
                                 , slProgram :: String
                                 , slAddress :: SockAddr }

openlog :: HostName        -- ^ Remote hostname, or localhost
        -> String          -- ^ Port number or name, default is 514
        -> String          -- ^ Name to log under
        -> IO SyslogHandle -- ^ Handle to use for logging
openlog hostname port progname = do
  {-
   - Look up the hostname and port. Either raises an exception or returns a
   - nonempty list. First element in that list is supposed to be the best
   - option.
   -}
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  -- Establish a socket for communication
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  -- Save off the socket, program name, and server address in a handle
  return $ SyslogHandle sock progname $ addrAddress serveraddr

-- Log the 'String' message to the 'SyslogHandle'
syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
-- syslog syslogh fac pri msg = sendstr sendmsg
syslog syslogh fac pri msg = sendstr $ Strict.pack sendmsg
  where code    = makeCode fac pri
        sendmsg = "<" ++ show code ++ ">" ++ slProgram syslogh ++
                  ": " ++ msg

        -- Send until everything is done
        sendstr :: Strict.ByteString -> IO ()
        sendstr omsg = unless (Strict.null omsg) $ do
                         sent <- sendTo (slSocket syslogh) omsg
                                 (slAddress syslogh)
                         sendstr $ Strict.drop (fromIntegral sent) omsg

closelog :: SyslogHandle -> IO ()
closelog syslogh = close $ slSocket syslogh

{- | Convert a facility and a priority into a syslog code -}
makeCode :: Facility -> Priority -> Int
makeCode fac pri =
  let faccode = codeOfFac fac
      pricode = fromEnum pri
  in  (faccode `shiftL` 3) .|. pricode
