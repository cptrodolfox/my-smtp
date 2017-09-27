module Sender where

import qualified Codes                     as Code
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import qualified Types                     as T

--Start Connection to socket
startConn :: HostName -> ServiceName -> IO Socket
startConn host service = do
  addrInfos <- getAddrInfo Nothing (Just host) (Just service)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  return sock

--


errorMessage :: Int -> T.ForwardPath -> T.Domain -> T.Command -> Socket -> IO ()
errorMessage code fp dom cmm sock = sendAll sock $ Code.codeMessage code fp dom cmm
