{-
 - UDP servers bind to a specific port on the server machine. They will
 - accept packets directed to that port and process them. Since UDP is
 - a stateless and packet-oriented protocol, programmers normall use a call
 - such as 'recvFrom' to receive both the data and information about the
 - machine that sent it, which is used for sending back a response.
 -}

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as Strict

import Data.Bits
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (recvFrom)
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String        -- ^ Port number or name, 514 is the default
         -> HandlerFunc   -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $ do
    {-
     - Look up the port, either raises an exception or returns a nonempty
     - list.
     -}
    addrinfos <- getAddrInfo
                 (Just $ defaultHints { addrFlags = [AI_PASSIVE] })
                 Nothing $ Just port

    let serveraddr = head addrinfos

    -- Create a socket
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

    -- Bind it to the address we are listening to
    bind sock $ addrAddress serveraddr

    {-
     - Loop forever processing incoming data, only "Ctrl-C" can be used to
     - abort
     -}
    procMessages sock

  where procMessages sock = forever $ do
          {-
           - Receive one UDP packet, max length of 1024 bytes, and save its
           - contents into 'msg' and its source IP and port into 'addr'
           -}
          (msg, addr) <- recvFrom sock 1024

          -- Call 'handlerfunc' to handle the message
          handlerfunc addr $ Strict.unpack msg

          -- And process more messages (infinite loop)
          -- procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg

-- Simple test main function
main :: IO ()
main = serveLog "1514" plainHandler
