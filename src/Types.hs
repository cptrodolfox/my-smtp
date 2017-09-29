{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import qualified Data.ByteString.Char8 as Char8
import           Data.Time.Clock
import           Data.Typeable
--Type for the ForwardPath as ByteString since transmission
--would be done by transmitting bytestring
newtype ForwardPath = ForwardPath Char8.ByteString
  deriving (Eq, Show)

--Type for the Domain as ByteString since transmission
--would be done by transmitting bytestring
newtype Domain = Domain Char8.ByteString
  deriving (Eq, Show)

--Type to identify the different SMTP commands
newtype Command = Command Int
  deriving (Eq, Show)

newtype Username = Username Char8.ByteString
  deriving (Eq, Show, Typeable, Ord)

newtype MailBody = MailBody Char8.ByteString
  deriving (Eq, Show, Typeable)

newtype MailBox = MailBox [Email]
  deriving (Show, Typeable)

-- ADT to represent and email
data Email = Email { from :: !Username
                   , body :: !MailBody
                   , time :: !UTCTime }
             deriving (Show, Typeable)



--Unwraps ForwardPath
unForwardPath :: ForwardPath -> Char8.ByteString
unForwardPath (ForwardPath x) = x

--Unwraps Domain
unDomain ::  Domain -> Char8.ByteString
unDomain (Domain x) = x
