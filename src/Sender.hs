module Sender where

import qualified Codes                     as Code
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import qualified Types                     as T

errorMessage :: Int -> T.ForwardPath -> T.Domain -> T.Command -> Socket -> IO ()
errorMessage code fp dom cmm sock = sendAll sock $ Code.codeMessage code fp dom cmm
