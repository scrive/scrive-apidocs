{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Templates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- This is main templating helper. There are some utils for rendering standard templates and
-- standard data wrappers.
-----------------------------------------------------------------------------
module Templates.TemplatesUtils
    (wrapHTML,renderActionButton,option,soption,Option(..)) where


import KontraLink 
import Templates.Templates
import Data.Typeable
import Data.Data

{- Common templates - should be shared and it seams like a good place fo them -}
wrapHTML::KontrakcjaTemplates  -> String->IO String
wrapHTML templates body =  renderTemplate templates  "wrapHTML" [("body",body)]


renderActionButton::KontrakcjaTemplates  -> KontraLink -> String -> IO String
renderActionButton templates  action button = do
                                        buttonname <- renderTemplate templates  button []
                                        renderTemplate templates "actionButton" [("action",show action),("buttonname",buttonname)]



{- Option one of standard wrappers. It holds two strings and bool. 
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
{- Unselected option easy generation-}
option::(a->String)->(a->String)->a->Option
option f g x = Option {oValue=f x, oText=g x, oSelected = False}

{- Selected option easy generation-}
soption::(a->String)->(a->String)->a->Option
soption f g x= Option {oValue=f x, oText=g x, oSelected = True}
