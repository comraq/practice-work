{-
 - With TCP, connections are stateful. This means that there is a dedicated
 - logical "channel" between a client and server, rather than just one-off
 - packets as with UDP. This makes things easy for client developers, but a
 - bit more work for server side applications, as the server now has to
 - concurrently handle multiple connections at once.
 -
 - To do this, first create a socket and bind to a port. Instead of
 - repeatedly listening for data from any location, the main loop will be
 - around the 'accept' call. Each time a client connects, the server's
 - operating system allocates a new socket for it. So we have the 'master'
 - socket, used only to listen for incoming connections, and never to
 - transmit data. We also have the potential for multiple 'child' sockets to
 - be used at once, each corresponding to a logical TCP conversation.
 -
 - In Haskell, 'forkIO' will be used to create lightweight threads to handle
 - each connection.
 -}

 {-
  - Suppose now that a single message is not defined to be a single packet,
  - but is ended by a trailing newline character '\n'. Any given client
  - could send 0 or more messages to the server using a given TCP
  - connection.
  -}

import Control.Monad (forever)

import Network.Socket
import Control.Concurrent
import System.IO

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
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    -- Bind it to the address we are listening to
    bind sock $ addrAddress serveraddr

    {-
     - Start listening for connection request. We set the maximum queue size
     - to be 5, ie: at most 5 connection requests can be handled at once.
     -}
    listen sock 5

    -- Create a lock to use for synchronizing access to the handler
    lock <- newMVar ()

    -- Loop forever waiting for connections. Again only 'Ctrl-C' can be used
    -- to quit
    procRequests lock sock

  where
    -- | Process incoming connection requests
    procRequests :: MVar () -> Socket -> IO ()
    procRequests lock mastersock = forever $ do
      (connsock, clientaddr) <- accept mastersock
      handle lock clientaddr "syslogtcpserver.hs: client connected"

      forkIO $ procMessages lock connsock clientaddr

    -- | Process incoming messages per connection
    procMessages :: MVar () -> Socket -> SockAddr -> IO ()
    procMessages lock connsock clientaddr = do
      {-
       - Note that we converted the TCP socket into a Haskel 'Handle', also
       - explicitly setting the handle's buffering.
       -
       - After lazily reading all of the data from the socket's 'Handle', we know
       - that the remote has closed the socket
       -}
      connhdl <- socketToHandle connsock ReadMode
      hSetBuffering connhdl LineBuffering
      messages <- hGetContents connhdl
      mapM_ (handle lock clientaddr) (lines messages)
      hClose connhdl
      handle lock clientaddr "syslogtcpserver.hs: client disconnected"

    -- Lock the handler before passing data to it
    handle :: MVar () -> HandlerFunc
      {-
       - Note that this is the same type as:
       -   handle :: MVar () -> SockAddr -> String -> IO ()
       -
       - Since we are handling multiple messages concurrently, we use the
       - 'MVar' as a lock to prevent outputing garbled server response messages
       -}
    handle lock clientaddr msg = withMVar lock $ \a ->
      handlerfunc clientaddr msg >> return a


-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg

-- Simple test main function
main :: IO ()
main = serveLog "10514" plainHandler
