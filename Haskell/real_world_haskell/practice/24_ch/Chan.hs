import Control.Concurrent
import Control.Concurrent.Chan

{-
 - Channels only supports one-way communication because the receive of a
 - channel cannot respond to the sender. (A second channel is required if
 - two-way communication is desired)
 -}
chanExample :: IO ()
chanExample = do
  ch <- newChan
  forkIO $ do
    writeChan ch "hello world"
    writeChan ch "now i quit"

  readChan ch >>= print
  readChan ch >>= print

{-
 - If a 'Chan' is empty, 'readChan' blocks until there is value to be read.
 - ie: the receiver blocks until it receives a message from the channel
 -
 - 'writeChan' never blocks, always writing immediately.
 - ie: since the sender cannot receive any message, it is given the freedom
 -     to immediately write/send any message across the channel.
 -}

{-
 - Unbounded Channels:
 -
 - Since 'writeChan' always succeeds immediately, there is a potential risk
 - of the sending thread writing more often to a 'Chan' the the receiving
 - thread trying to read from it. The unread messages can pile up and
 - consume too much memory!
 -}
