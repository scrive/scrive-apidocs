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
import Data.Data
import Templates.Templates
import Mails.SendMail
import Data.Char
import Kontra
import User.Lang
import qualified Templates.Fields as F

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
    oValue:: String
  , oText:: String
  , oSelected :: Bool
  } deriving (Data, Typeable)

{- | Unselected option easy generation-}
option :: (a -> String) -> (a -> String) -> a -> Option
option f g x = Option { oValue = f x, oText = g x, oSelected = False }

{- | Selected option easy generation-}
soption :: (a -> String) -> (a -> String) -> a -> Option
soption f g x = Option { oValue = f x, oText = g x, oSelected = True }

{-| USEFULL. Marks list of fields with even and odd values. Counts from 1!-}
markParity :: Monad m => [Fields m ()] -> [Fields m ()]
markParity (f0:(f1:fs)) = [markOdd f0, markEven f1] ++ markParity fs
markParity (f:[]) = [markOdd f]
markParity [] = []

markEven :: Monad m => Fields m () -> Fields m ()
markEven fields = do
  fields
  F.value "even" True
  F.value "odd"  False

markOdd :: Monad m => Fields m () -> Fields m ()
markOdd fields = do
  fields
  F.value "even" False
  F.value "odd"  True

kontramaillocal :: (HasLang a, TemplatesMonad m) => a -> String -> Fields m () -> m Mail
kontramaillocal = kontramailHelper . renderLocalTemplate

kontramail :: TemplatesMonad m  => String -> Fields m () -> m Mail
kontramail = kontramailHelper renderTemplate

kontramailHelper :: TemplatesMonad m
                 => (String -> Fields m () -> m String)
                 -> String
                 -> Fields m ()
                 -> m Mail
kontramailHelper renderFunc tname fields = do
    -- Log.debug "Mail rendering by kontramail'"
    wholemail <- renderFunc tname fields
    let (title,content) = span (/= '\n') $ dropWhile (isControl ||^ isSpace) wholemail
    -- Log.debug $ "kontramail'->Title:" ++ title
    return $ emptyMail  {   title   = title
                          , content = content
                        }

contextFields :: Monad m => Context -> Fields m ()
contextFields ctx = do
  F.value "ctxhostpart" (ctxhostpart ctx)
  F.value "ctxlang" (codeFromLang $ ctxlang ctx)

