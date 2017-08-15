-- Derived from https://wiki.haskell.org/Implement_a_chat_server
--
-- Exercise: The code below prints the first line of an incoming HTTP request
-- on http://localhost:5555 and returns a canned response. Modify the code
-- to return a different message depending on the request method used.
--
-- Run with:
--      
--      stack runhaskell server.hs
--
-- Check the result at http://localhost:5555 with your browser. You may 
-- need to ensure that this port is clear before running.


{-# LANGUAGE OverloadedStrings #-}


import Network.Socket
import qualified Network.Socket.ByteString as BS
import qualified Data.ByteString.Char8 as C

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    request <- BS.recv sock 4096
    putStrLn $ takeWhile (\x -> x /= '\r') (C.unpack request)
    BS.send sock "HTTP/1.0 200 OK\r\n\r\nHello, World!\r\n"
    close sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat
 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 5555 iNADDR_ANY)   -- listen on TCP port 5555.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock
