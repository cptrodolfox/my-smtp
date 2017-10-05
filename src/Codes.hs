module Codes where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict       as Map
import qualified Types                 as T


--This function translates the different error codes to human
--readable messages.
codeMessage :: Int -> T.ForwardPath -> T.Domain -> T.Command -> Char8.ByteString
codeMessage n fp d a
  | n == 500 = Char8.pack "Syntax error, command unrecognized."
  | n == 501 = Char8.pack "Syntax error in parameters or arguments."
  | n == 502 = Char8.pack "Command not implemented."
  | n == 503 = Char8.pack "Bad sequence of commands."
  | n == 504 = Char8.pack "Command parameter not implemented."
  | n == 211 = Char8.pack "System status, or system help reply."
  | n == 214 = Char8.append (Char8.pack "Help message: ") (commandHelp a)
  | n == 220 = Char8.append (T.unDomain d) (Char8.pack " Service Ready.")
  | n == 221 = Char8.append (T.unDomain d) (Char8.pack " Service closing transmission channel.")
  | n == 421 = Char8.append (T.unDomain d) (Char8.pack " Service not available, transmission channel.")
  | n == 250 = Char8.pack "Requested mail action okay, completed."
  | n == 251 = Char8.append (Char8.pack "User not local; will forward to ") (T.unForwardPath fp)
  | n == 450 = Char8.pack "Requested mail action not taken: mailbox unavailable."
  | n == 550 = Char8.pack "Requested action not taken: mailbox unavailable."
  | n == 451 = Char8.pack "Requested action aborted: error in processing."
  | n == 551 = Char8.append (Char8.pack "User not local; please try ") (T.unForwardPath fp)
  | n == 452 = Char8.pack "Requested action not taken: insufficient system storage."
  | n == 552 = Char8.pack "Requested mail action aborted: exceeded storage allocation."
  | n == 553 = Char8.pack "Requested action not taken: mailbox name not allowed."
  | n == 354 = Char8.pack "Start mail input; end with <CRLF>.<CRLF>."
  | n == 554 = Char8.pack "Transaction failed."
  | otherwise = Char8.pack "Message Unknown"

--This function will output the help of every command in the SMTP RFC 821
commandHelp :: T.Command -> Char8.ByteString
commandHelp _ = Char8.pack "TODO"
