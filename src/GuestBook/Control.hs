{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module GuestBook.Control where

import Control.Applicative((<$>))
import Control.Monad(msum)
import Control.Monad.Trans(liftIO)
import Data.ByteString.Lazy.UTF8 (toString)
import GuestBook.State (GuestBookEntry(..),
                        AddGuestBookEntry(..),
                        ReadGuestBook(..),
                        Login(..))
import GuestBook.View
import Happstack.Server
import Happstack.Data(defaultValue)
import Happstack.State(query,update)
import HSP
--import System.Time(getClockTime)
import Control.Monad

guestBookHandler :: ServerPartT IO (HSP XML)
guestBookHandler = msum 
                   [ dir "entries" $ msum [postEntry, getEntries]
                   , dir "login" $ msum [postLogin]
                   ]

postLogin :: ServerPartT IO (HSP XML)
postLogin = methodM POST >> do -- only accept a post method
  loginEntry <- getData -- get the data
  case loginEntry of 
    Nothing -> error $ "error: postLogin" 
    Just (Login name _password) -> do
       ok $ welcome name -- seeOther "/entries" (seeOtherXML "/entries")

postEntry :: ServerPartT IO (HSP XML)
postEntry = methodM POST >> do -- only accept a post method
  mbEntry <- getData -- get the data
  case mbEntry of 
    Nothing -> error $ "error: postEntry" 
    Just entry -> do
      -- now <- liftIO getClockTime
      update $ AddGuestBookEntry entry{date=1}
      seeOther "/entries" (seeOtherXML "/entries")

-- |show all the entries in the guestbook
-- argument is a callback function 
getEntries :: ServerPartT IO (HSP XML)
getEntries = 
    methodM GET >> 
                do gb  <- query ReadGuestBook
                   ok $ <div><% gb %></div> -- FIXME: remove <div />

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData GuestBookEntry where
  fromData = do
    author  <- look "author" `mplus` (error "GuestBookEntry, need author")
    message <- look "message" `mplus` (error "GuesBookEntry, need message")
    email   <- look "email" `mplus` (error "GuestBookEntry: need email")
    return $ GuestBookEntry (if null author then "Anonymous" else author) message defaultValue email

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData Login where
  fromData = do
    name  <- look "email" `mplus` (error "Need your email address")
    password <- look "password" `mplus` (error "Need your password")
    return $ Login name password 
