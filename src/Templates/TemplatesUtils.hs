-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Templates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- LOT OF STUFF IN THIS FILE ARE DEPRECIATED. EXCEPT ONES THAT ARE MARKED AS USEFULL TRY NOT TO USE THEM.
-- This is main templating helper. There are some utils for rendering standard templates and
-- standard data wrappers.
-----------------------------------------------------------------------------
module Templates.TemplatesUtils
(option,soption,Option(..),markParity,kontramail,kontramaillocal,contextFields) where

import Control.Logic
import Control.Monad.IO.Class
import Data.Data
import Templates.Templates
import Mails.SendMail
import Data.Char
import Kontra
import User.Locale

{- Common templates - should be shared and it seams like a good place for them -}

{- |
   Wrap an HTML string so that it becomes the body of an HTML document
 -}

{- |
   Option one of standard wrappers. It holds two strings and bool.
   Quick note on how to use wrappers:
    (1) Lets say we want to print select a user select box
    (2) We take users list and  map over it with option userid username
    (3) We pass the result of (2) to templates with name users
    (4) In template we call $users:{n| <option value="$n.oValue$">$n.oText$</option>}$
    (4') Or even better -> we call $users:options()$
 -}
data Option = Option {
              oValue:: String,
              oText:: String,
              oSelected :: Bool
            } deriving (Data, Typeable)

{- | Unselected option easy generation-}
option::(a->String)->(a->String)->a->Option
option f g x = Option {oValue=f x, oText=g x, oSelected = False}

{- | Selected option easy generation-}
soption::(a->String)->(a->String)->a->Option
soption f g x= Option {oValue=f x, oText=g x, oSelected = True}

{-| USEFULL. Marks list off fields with even and odd values. Caunts from 1!-}
markParity :: MonadIO m => [Fields m] -> [Fields m]
markParity (f0:(f1:fs)) = [markOdd f0 ,markEven f1]++markParity fs
markParity (f:[]) = [markOdd f]
markParity [] = []

markEven :: MonadIO m => Fields m -> Fields m
markEven f = do
    f
    field "even" True
    field "odd"  False

markOdd :: MonadIO m => Fields m -> Fields m
markOdd f = do
    f
    field "even" False
    field "odd"  True


kontramaillocal :: (HasLocale a, TemplatesMonad m) => a -> String -> Fields m -> m Mail
kontramaillocal haslocale = kontramail' (renderLocalTemplateFM haslocale)

kontramail :: TemplatesMonad m  => String -> Fields m -> m Mail
kontramail = kontramail' renderTemplateFM

kontramail':: TemplatesMonad m
              => (String -> Fields m -> m String)
              -> String
              -> Fields m
              -> m Mail
kontramail' renderFunc tname fields = do
    -- Log.debug "Mail rendering by kontramail'"
    wholemail <- renderFunc tname fields
    let (title,content) = span (/= '\n') $ dropWhile (isControl ||^ isSpace) wholemail
    -- Log.debug $ "kontramail'->Title:" ++ title
    return $ emptyMail  {   title   = title
                          , content = content
                        }

contextFields ::(MonadIO m) => Context -> Fields m
contextFields ctx = field "ctxhostpart" (ctxhostpart ctx)