module Client where

import qualified Data.ByteString.Char8     as C
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

--- SMTP Commands ---

newtype Command = Command C.ByteString
commMailFrom = Command $ C.pack "MAIL FROM:"
commRcptTo = Command $ C.pack "RCPT TO:"
commData = Command $ C.pack "DATA"
commQuit = Command $ C.pack "QUIT"

--- SMTP Sucess Code ---
successCode = C.pack "250"

--- SMTP Finish Line ---
finishLine = C.pack "."

--- This function sends a commands over the socket and returns the response.
sendCommand :: Socket -> Command -> C.ByteString -> IO C.ByteString
sendCommand sock (Command com) input = do sendAll sock (C.append com input)
                                          C.putStrLn (C.append com input)
                                          res <- recv sock 1024
                                          return res

--- This function checks if a response is a success.
isSuccessful :: C.ByteString -> Bool
isSuccessful = (==) successCode . C.take 3

--- This function gets the data of a mail it stops with \n.\n
getData :: C.ByteString -> IO (C.ByteString)
getData line = do newline <- C.getLine
                  let lines = C.append line newline
                  if (line == finishLine)
                    then return lines
                    else getData lines


main :: IO ()
main = do putStrLn "Email server:"
          dest <- getLine
          addrinfos <- getAddrInfo Nothing (Just dest) (Just "25")
          let server = head addrinfos
          putStrLn "Connecting to server..."
          sock <- socket (addrFamily server) Stream defaultProtocol
          connect sock (addrAddress server)
          let helo = C.pack "HELO anyhostname"
          sendAll sock helo
          msg <- recv sock 1024
          C.putStrLn msg
          putStrLn "Mail from:"
          mailFrom <- C.getLine
          resp1 <- sendCommand sock commMailFrom mailFrom
          C.putStrLn resp1
          putStrLn "Mail to:"
          mailTo <- C.getLine
          resp2 <- sendCommand sock commRcptTo mailTo
          C.putStrLn resp2
          putStrLn "Data:"
          dat <- getData $ C.pack ""
          resp3 <- sendCommand sock commData dat
          C.putStrLn resp3
          resp4 <- sendCommand sock commQuit (C.pack "")
          C.putStrLn resp4
          close sock






