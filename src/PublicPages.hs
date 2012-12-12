{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module PublicPages (
    publicPages
  ) where


import AppView as V
import Kontra
import KontraLink
import LangRouting (allLangDirs, dirByLang)
import Redirect
import Routing
import Happstack.StaticRouting (Route, choice, dir)
import User.Model
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString)

import Templates.Templates
import Happstack.Server hiding (simpleHTTP, host, https, dir, path)

publicPages :: Route (KontraPlus Response)
publicPages = choice
     [ allLangDirs $ const $ hGetAllowHttp $ toK0 handleHomepage
     , publicDir "priser" "pricing" LinkPriceplan handlePriceplanPage
     , publicDir "sakerhet" "security" (\l -> LinkLogin l LoginTry) sendLoginRedirect -- Drop all this pages after Feb, 2013
     , publicDir "juridik" "legal" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "sekretesspolicy" "privacy-policy" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "allmana-villkor" "terms" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "om-scrive" "about" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "partners" "partners" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "kunder" "clients" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "kontakta" "contact" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "scriveapi" "scriveapi" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "scrivebymail" "scrivebymail" (\l -> LinkLogin l LoginTry) sendLoginRedirect
     , publicDir "funktioner" "features" (\l -> LinkLogin l LoginTry) sendLoginRedirect

     -- localization javascript
     , dir "localization"    $ allLangDirs $ \l -> hGetAllowHttp $ toK1 $ \(_::String) ->  generateLocalizationScript l
     ]
  where
    sendLoginRedirect = do
      ctx <- getContext
      sendRedirect (LinkLogin (ctxlang ctx) LoginTry)
{- |
    This is a helper function for routing a public dir.
-}
publicDir :: String -> String -> (Lang -> KontraLink) -> Kontra Response -> Route (KontraPlus Response)
publicDir swedish english link handler = choice $
  [
    -- the correct url with lang/publicdir where the publicdir must be in the correct lang
    allLangDirs $ \lang -> dirByLang lang swedish english $ hGetAllowHttp $ handler

    -- if they use the swedish name without lang we should redirect to the correct swedish lang
  , dir swedish $ hGetAllowHttp $ redirectKontraResponse $ link LANG_SV
  ] ++ if swedish == english
       then [] -- if prefixes are identical, we don't know to what lang we should redirect,
               -- so ignore English redirect
       else
  [
    -- if they use the english name without lang we should redirect to the correct english lang
    dir english $ hGetAllowHttp $ redirectKontraResponse $ link LANG_EN
  ]

handleHomepage :: Kontra KontraLink
handleHomepage = return LinkDesignView

handlePriceplanPage :: Kontra Response
handlePriceplanPage = priceplanPage



generateLocalizationScript :: Kontrakcja m => Lang -> m Response
generateLocalizationScript lang = do
   switchLang $ lang
   script <- renderTemplate_ "javascriptLocalisation"
   ok $ toResponseBS (BS.fromString "text/javascript;charset=utf-8") $ BSL.fromString script
