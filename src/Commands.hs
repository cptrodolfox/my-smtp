module Commands where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as Char8
import           Data.Char
import           Network.Socket                   hiding (recv, recvFrom, send,
                                                   sendTo)
import           Network.Socket.ByteString
import           Text.ParserCombinators.ReadP
import qualified Types                            as T
-----------------------------------
---Constants-----------------------
-----------------------------------
commandList :: [Char8.ByteString]
commandList = ["HELO"
              , "MAIL"
              , "RCPT"
              , "DATA"
              , "RSET"
              , "SEND"
              , "SOML"
              , "SAML"
              , "VRFY"
              , "EXPN"
              , "HELP"
              , "NOOP"
              , "QUIT"
              , "TURN"]
-----------------------------------
--- Parsers------------------------
-----------------------------------

-- A SMTP command parser
commandParser :: ReadP String
commandParser = do
  comm <- many1 (satisfy (\c -> c >= 'A' && c <= 'z'))
  satisfy (== ' ')
  return comm

-- A SMTP domain parser
domainParser :: ReadP String
domainParser = do
  dom <- many1 (satisfy (\c ->
                           (c >= 'A' && c <= 'z')
                           || c == '.'
                           || c == '-'
                           || c == '@'
                           || (c >= '0' && c <= '9')
                        ))
  satisfy (== ':')
  return dom

-- A SMTP mail parser
mailParser :: ReadP (String, String)
mailParser = do
  satisfy (== '<')
  mail <- many1 (satisfy (\c ->
                            (c >= 'A' && c <= 'z')
                            || c == '-'
                            || c == '.'
                            || (c >= '0' && c <= '9')
                         ))
  satisfy (=='@')
  dom <- many1 (satisfy (\c ->
                            (c >= 'A' && c <= 'z')
                            || c == '-'
                            || c == '.'
                         ))
  satisfy (=='>')
  return (mail, dom)


--------------------------------------------
------------Functions-----------------------
--------------------------------------------


--The SMTP helo command as defined by RFC 821
helo :: String -> Char8.ByteString
helo x = Char8.append (Char8.pack "HELO ") (Char8.pack x)

--The SMTP mail command as defined by RFC 821
mail :: String -> Char8.ByteString
mail x = Char8.append (Char8.pack "MAIL FROM:") (Char8.pack x)

--The SMTP rcpt command as defined by FRC 821
recipient :: String -> Char8.ByteString
recipient x = Char8.append (Char8.pack "RCPT TO:") (Char8.pack x)

--The SMTP data command as defined by RFC 821
--the data ends with <CRFL>.<CRFL>
dataComm :: String -> Char8.ByteString
dataComm x = Char8.append (Char8.pack "DATA ") (Char8.pack x)

--The SMTP reset command as defined by RFC 821
reset :: String -> Char8.ByteString
reset _ = Char8.pack "RSET "

--The SMTP send command as defined by RFC 821
send :: String -> Char8.ByteString
send x = Char8.append (Char8.pack "SEND FROM:") (Char8.pack x)

--The SMTP send or mail command defined by RFC 821
sendOrMail :: String -> Char8.ByteString
sendOrMail x = Char8.append (Char8.pack "SOML FROM:") (Char8.pack x)

--The SMTP send and mail command defined by RFC 821
sendAndMail :: String -> Char8.ByteString
sendAndMail x = Char8.append (Char8.pack "SAML FROM:") (Char8.pack x)

--The SMTP verify command defined by RFC 821
verify :: String -> Char8.ByteString
verify x = Char8.append (Char8.pack "VRFY ") (Char8.pack x)

--The SMTP expand command defined by RFC 821
expand :: String -> Char8.ByteString
expand x = Char8.append (Char8.pack "EXPN ") (Char8.pack x)

--The SMTP help command defined by RFC 821
help :: String -> Char8.ByteString
help x = Char8.append (Char8.pack "HELP ") (Char8.pack x)

--The SMTP no operation defined by RFC 821
noop :: String -> Char8.ByteString
noop _ = Char8.pack "NOOP "

--The SMTP quit command defined by RFC 821
quit :: String -> Char8.ByteString
quit _ = Char8.pack "QUIT "

--The SMTP turn command defined by RFC 821
turn :: String -> Char8.ByteString
turn _ = Char8.pack "TURN "



--This function parses a meesage to obtain the
--SMTP command.
getCommand :: Char8.ByteString -> (String, Char8.ByteString)
getCommand x =
  let msg = Char8.unpack x
  in (\(a, b) -> (a, Char8.pack b)) . head $ readP_to_S commandParser msg

--This function checks whether the string is a valid
--SMTP command
isCommand :: Char8.ByteString -> Bool
isCommand x = let upX = map toUpper x
              in elem upX commandList


--Start Connection to socket
startConn :: HostName -> ServiceName -> IO Socket
startConn host service = do
  addrInfos <- getAddrInfo Nothing (Just host) (Just service)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  return sock
