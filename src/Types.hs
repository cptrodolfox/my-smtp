module Types where

import qualified Data.ByteString.Char8 as Char8

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

--Unwraps ForwardPath
unForwardPath :: ForwardPath -> Char8.ByteString
unForwardPath (ForwardPath x) = x

--Unwraps Domain
unDomain ::  Domain -> Char8.ByteString
unDomain (Domain x) = x
