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
import Happstack.Fields
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
     , publicDir "sakerhet" "security" LinkSecurity handleSecurityPage
     , publicDir "juridik" "legal" LinkLegal handleLegalPage
     , publicDir "sekretesspolicy" "privacy-policy" LinkPrivacyPolicy handlePrivacyPolicyPage
     , publicDir "allmana-villkor" "terms" LinkTerms handleTermsPage
     , publicDir "jobb" "jobs" LinkJobs handleJobsPage
     , publicDir "om-scrive" "about" LinkAbout handleAboutPage
     , publicDir "partners" "partners" LinkPartners handlePartnersPage
     , publicDir "kunder" "clients" LinkClients handleClientsPage
     , publicDir "kontakta" "contact" LinkContactUs handleContactUsPage
     , publicDir "scriveapi" "scriveapi" LinkAPIPage handleApiPage
     , publicDir "scrivebymail" "scrivebymail" LinkScriveByMailPage handleScriveByMailPage
     , publicDir "funktioner" "features" LinkFeaturesPage handleFeaturesPage

     -- sitemap
     , dir "webbkarta"       $ hGetAllowHttp $ handleSitemapPage
     , dir "sitemap"         $ hGetAllowHttp $ handleSitemapPage
     -- localization javascript
     , dir "localization"    $ allLangDirs $ \l -> hGetAllowHttp $ toK1 $ \(_::String) ->  generateLocalizationScript l
     ]

{- |
    This is a helper function for routing a public dir.
-}
publicDir :: String -> String -> (Lang -> KontraLink) -> Kontra Response -> Route (KontraPlus Response)
publicDir swedish english link handler = choice $
  [
    -- the correct url with lang/publicdir where the publicdir must be in the correct lang
    allLangDirs $ \lang -> dirByLang lang swedish english $ hGetAllowHttp $ handler

    -- if they use the swedish name without lang we should redirect to the correct swedish lang
  , dir swedish $ hGetAllowHttp $ redirectKontraResponse $ link LANG_SE
  ] ++ if swedish == english
       then [] -- if prefixes are identical, we don't know to what lang we should redirect,
               -- so ignore English redirect
       else
  [
    -- if they use the english name without lang we should redirect to the correct english lang
    dir english $ hGetAllowHttp $ redirectKontraResponse $ link LANG_EN
  ]

handleHomepage :: Kontra (Either Response (Either KontraLink String))
handleHomepage = do
  ctx@Context{ctxmaybeuser} <- getContext
  loginOn <- isFieldSet "logging"
  referer <- getField "referer"
  email   <- getField "email"
  case ctxmaybeuser of
    Just _user -> do
      response <- V.simpleResponse =<< firstPage ctx loginOn referer email
      clearFlashMsgs
      return $ Left response
    Nothing -> do
      response <- V.simpleResponse =<< firstPage ctx loginOn referer email
      clearFlashMsgs
      return $ Left response

handleSitemapPage :: Kontra Response
handleSitemapPage = handleWholePage sitemapPage

handlePriceplanPage :: Kontra Response
handlePriceplanPage = handleWholePage priceplanPage

handleSecurityPage :: Kontra Response
handleSecurityPage = handleWholePage securityPage

handleLegalPage :: Kontra Response
handleLegalPage = handleWholePage legalPage

handlePrivacyPolicyPage :: Kontra Response
handlePrivacyPolicyPage = handleWholePage privacyPolicyPage

handleTermsPage :: Kontra Response
handleTermsPage = handleWholePage termsPage

handleJobsPage :: Kontra Response
handleJobsPage = handleWholePage jobsPage

handleAboutPage :: Kontra Response
handleAboutPage = handleWholePage aboutPage

handlePartnersPage :: Kontra Response
handlePartnersPage = handleWholePage partnersPage

handleClientsPage :: Kontra Response
handleClientsPage = handleWholePage clientsPage

handleContactUsPage :: Kontra Response
handleContactUsPage = handleWholePage contactUsPage

handleApiPage :: Kontra Response
handleApiPage = handleWholePage apiPage

handleScriveByMailPage :: Kontra Response
handleScriveByMailPage = handleWholePage scriveByMailPage

handleFeaturesPage :: Kontra Response
handleFeaturesPage = handleWholePage featuresPage

handleWholePage :: Kontra String -> Kontra Response
handleWholePage f = do
  content <- f
  response <- V.simpleResponse content
  clearFlashMsgs
  return response

generateLocalizationScript :: Kontrakcja m => Lang -> m Response
generateLocalizationScript lang = do
   switchLang $ lang
   script <- renderTemplate_ "javascriptLocalisation"
   ok $ toResponseBS (BS.fromString "text/javascript;charset=utf-8") $ BSL.fromString script
