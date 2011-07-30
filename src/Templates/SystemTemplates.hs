{-# LANGUAGE OverlappingInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.SystemTemplates
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------
module Templates.SystemTemplates
    (
        getSystemTemplates
    ) where

import User.SystemServer

getSystemTemplates :: SystemServer -> IO [(String,String)]
getSystemTemplates s = return $ map (\(n,f) -> (n,f s)) [
   ("ourFirstPageLogo",ourFirstPageLogo)
  ,("ourLogoClass",ourLogoClass)
  ,("ourFirstPageLogo",ourFirstPageLogo)
  ,("ourServiceName",ourServiceName)
  ,("ourServiceFullName",ourServiceFullName)
  ,("ourServiceAdress",ourServiceAdress)
  ,("ourServicePhone",ourServicePhone)
  ,("ourServiceMail",ourServiceMail)
 ]

ourFirstPageLogo :: SystemServer -> String
ourFirstPageLogo SkrivaPa    = "/img/logo-skrivapa.png"
ourFirstPageLogo Scrive = "/img/logo-scrive.png"

ourLogoClass :: SystemServer -> String
ourLogoClass SkrivaPa    = "skrivapa-logo"
ourLogoClass Scrive = "scrive-logo"

ourServiceName :: SystemServer -> String
ourServiceName SkrivaPa    = "SkrivaPå"
ourServiceName Scrive = "Scrive"

ourServiceFullName :: SystemServer -> String
ourServiceFullName SkrivaPa    = "SkrivaPå CM AB"
ourServiceFullName Scrive = "Scrive"

ourServiceAdress :: SystemServer -> String
ourServiceAdress SkrivaPa    = "SkrivaPå CM AB <BR/>Saltmätargatan 19<BR/>113 59 Stockholm"
ourServiceAdress Scrive = "Scrive CM AB <BR/>Saltmätargatan 19<BR/>113 59 Stockholm"

ourServicePhone :: SystemServer -> String
ourServicePhone SkrivaPa    = "010-195 99 15"
ourServicePhone Scrive = "+46 (0)10-195 99 15"

ourServiceMail :: SystemServer -> String
ourServiceMail SkrivaPa    = "info@skrivapa.se"
ourServiceMail Scrive = "info@scrive.com"
