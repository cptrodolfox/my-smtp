{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Receiver where

import           Control.Applicative
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (modify)
import           Data.Acid
import qualified Data.ByteString.Char8 as Char8
import           Data.Function
import qualified Data.IntMap           as IntMap
import qualified Data.Map              as Map
import           Data.SafeCopy
import           Data.Time
import           Data.Typeable
import           Types                 as T

-- The database holding the users and
-- mailboxes.
data MailDb = MailDb {allmails :: Map.Map T.Username T.MailBox}


-- The derive safe copy of the different types

deriveSafeCopy 0 'base ''T.Username
deriveSafeCopy 0 'base ''T.MailBox
deriveSafeCopy 0 'base ''T.Email
deriveSafeCopy 0 'base ''T.MailBody
deriveSafeCopy 0 'base ''MailDb


-- Adds a user to the database with an
-- empty mailboxes.
addUser :: T.Username -> Update MailDb ()
addUser user = modify go
  where
    go (MailDb db) = MailDb $
      if Map.null db
      then Map.singleton user (T.MailBox [])
      else Map.insert user (T.MailBox []) db

-- Adds an email to a users mailbox,
-- the existance of the user in the
-- database must be checked beforehand.
addMail :: T.Email -> Update MailDb ()
addMail email@(T.Email _ user _ _) = modify go
  where
    go (MailDb db) = MailDb $
      Map.update (\(T.MailBox emails)
                  -> Just (T.MailBox (email:emails))) user db

-- Gets the mail box of a user.
getMail :: T.Username -> Query MailDb T.MailBox
getMail user = (\(Just mailbox) -> mailbox)
  . (Map.lookup user)
  . allmails <$> ask


showUsersAndMailBoxes :: Query MailDb [(T.Username, T.MailBox)]
showUsersAndMailBoxes = Map.toList . allmails <$> ask

showUsers :: Query MailDb [T.Username]
showUsers = (map fst) . Map.toList . allmails <$> ask

isUser :: T.Username -> Query MailDb Bool
isUser user = (elem user). (map fst) . Map.toList . allmails <$> ask

-- Acid stuff.
makeAcidic ''MailDb ['showUsersAndMailBoxes
                    , 'showUsers
                    , 'addUser
                    , 'isUser
                    , 'addMail
                    , 'getMail]


main :: IO ()
main = do
  state <- openLocalState (MailDb Map.empty)
  allUsers <- query state ShowUsers
  mapM_ print allUsers
  Char8.putStrLn (Char8.pack "Please write name of username:")
  username <- Char8.getLine
  Char8.putStrLn (Char8.pack "Please write body mail")
  text <- Char8.getLine
  now <- getCurrentTime
  update state (AddMail $ T.Email (T.Username username) (T.Username username) (T.MailBody text) now)
  mailBox <- query state (GetMail (T.Username username))
  print mailBox



