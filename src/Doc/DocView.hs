{-# LANGUAGE ExtendedDefaultRules #-}
module Doc.DocView (
    pageCreateFromTemplate
  , documentInfoFields
  , mailDocumentAwaitingForAuthor
  , mailDocumentClosed
  , mailDocumentRejected
  , mailDocumentRemind
  , mailInvitation
  , mailForwardSigned
  , pageDocumentDesign
  , pageDocumentView
  , pageDocumentSignView
  , pageDocumentSignForPadView
  , pageDocumentPadList
  , pageDocumentPadListLogin
  , pagePostSignview
  , gtVerificationPage
  , documentSignviewBrandingCSS
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Happstack.Server.SimpleHTTP
import System.Exit
import Text.StringTemplates.Templates
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import AppView (kontrakcja, standardPageFields, brandingFields, companyUIForPage, renderFromBody)
import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import Kontra
import User.Model
import Utils.Color
import Utils.Font
import Utils.IO
import qualified Log as Log

pageCreateFromTemplate :: TemplatesMonad m => m String
pageCreateFromTemplate = renderTemplate_ "createFromTemplatePage"

pageDocumentDesign :: Kontrakcja m
                   => Context
                   -> Document
                   -> AnalyticsData
                   -> m String
pageDocumentDesign ctx document ad = do
     let  mbd = ctxbrandeddomain ctx
     mcompany <- companyUIForPage
     renderTemplate "pageDocumentDesign" $ do
         F.value "documentid" $ show $ documentid document
         standardPageFields ctx kontrakcja ad
         brandingFields mbd mcompany

pageDocumentView :: TemplatesMonad m
                    => Document
                    -> Maybe SignatoryLink
                    -> Bool
                    -> m String
pageDocumentView document msiglink authorcompanyadmin =
  renderTemplate "pageDocumentView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "authorcompanyadmin" $ authorcompanyadmin


pageDocumentSignView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignView ctx document siglink ad = do
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "usestandardheaders" $ (isJust $ maybesignatory siglink) && (maybesignatory siglink) == (userid <$> ctxmaybeuser ctx)
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany


pageDocumentSignForPadView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignForPadView ctx document siglink ad = do
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignPadView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany


pageDocumentPadList:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentPadList ctx ad = do
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pagePadListView" $ do
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany

pageDocumentPadListLogin:: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pageDocumentPadListLogin ctx ad = do
  let  mbd = ctxbrandeddomain ctx
  renderTemplate "padLogin" $ do
      standardPageFields ctx kontrakcja ad
      F.value "servicelinkcolour" $ bdservicelinkcolour <$> mbd
      F.value "textscolour" $ bdexternaltextcolour <$> mbd
      F.value "background" $ bdbackgroundcolorexternal <$> mbd
      brandingFields mbd Nothing

pagePostSignview :: Kontrakcja m
                    => Context
                    -> AnalyticsData
                    -> m String
pagePostSignview ctx ad = do
  let  mbd = ctxbrandeddomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pagePostSignview" $ do
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany


-- | Basic info about document , name, id ,author
documentInfoFields :: Monad m => Document -> Fields m ()
documentInfoFields  document  = do
  F.value "documenttitle" $ documenttitle document
  F.value "title" $ documenttitle document
  F.value "name" $ documenttitle document
  F.value "id" $ show $ documentid document
  F.value "documentid" $ show $ documentid document
  F.value "template" $  isTemplate document
  F.value "hasanyattachments" $ not (null $ (documentauthorattachments document)) || not (null (concatMap signatoryattachments $ documentsignatorylinks document))
  documentStatusFields document

-- | Fields indication what is a document status
documentStatusFields :: Monad m => Document -> Fields m ()
documentStatusFields document = do
  F.value "preparation" $ documentstatus document == Preparation
  F.value "pending" $ documentstatus document == Pending
  F.value "cancel" $ documentstatus document == Canceled
  F.value "timedout" $ documentstatus document == Timedout
  F.value "rejected" $ documentstatus document == Rejected
  F.value "signed" $ documentstatus document == Closed
  -- awaitingauthor is used in old view in old template
  -- remove it when old view is removed
  -- currently it means: is the next turn for author to sign?
  F.value "awaitingauthor" $ canAuthorSignNow document

-- Page for GT verification
gtVerificationPage :: Kontrakcja m => m Response
gtVerificationPage = renderFromBody kontrakcja =<< renderTemplate_ "gtVerificationPage"


-- Signview branding CSS. Generated using less
documentSignviewBrandingCSS :: (MonadIO m, Log.MonadLog m) => Maybe BrandedDomain -> Maybe CompanyUI -> m BSL.ByteString
documentSignviewBrandingCSS mbd mcui = do
    (code,stdout,stderr) <- do
      readProcessWithExitCode' "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ signviewBrandingLess mbd mcui)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          Log.attention_ $ "Creating sign view branding failed : " ++ BSL.toString stderr
          return BSL.empty


signviewBrandingLess :: Maybe BrandedDomain -> Maybe CompanyUI -> String
signviewBrandingLess mbd mcompanyui = unlines
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'signviewbranding/signviewbrandingdefaultvariables';", -- This will set default signview branding
    -- Following settings will overwrite default values
    bcolor "barscolor" $ (companysignviewbarscolour  =<< mcompanyui) `mplus` (bdbarscolour <$> mbd),
    bcolor "barstextcolor" $ (companysignviewbarstextcolour =<< mcompanyui) `mplus` (bdbarstextcolour <$> mbd),
    bcolor "textcolor" $  (companysignviewtextcolour =<< mcompanyui),
    bcolor "primarycolor" $ (companysignviewprimarycolour =<< mcompanyui) `mplus` (bdsignviewprimarycolour <$> mbd),
    bcolor "primarytextcolor" $ (companysignviewprimarytextcolour   =<< mcompanyui) `mplus` (bdsignviewprimarytextcolour <$> mbd),
    bcolor "secondarycolor" $ (companysignviewsecondarytextcolour   =<< mcompanyui) `mplus` (bdsignviewsecondarytextcolour <$> mbd),
    bcolor "secondarycolor" $ (companysignviewsecondarycolour   =<< mcompanyui) `mplus` (bdsignviewsecondarycolour <$> mbd),
    bcolor "backgroundcolor" $ (companysignviewbackgroundcolour =<< mcompanyui) `mplus` (bdbackgroundcolour <$> mbd),
    bfont "font" $ (companysignviewtextfont  =<< mcompanyui),
    -- Only last part will generate some css. Previews ones are just definitions
    "@import 'signviewbranding/signviewbranding';"
    ]
  where
    -- Some sanity checks on data. Note that this are provided by users
    bcolor :: String -> Maybe String -> String
    bcolor _ Nothing = ""
    bcolor n (Just c) = if (isValidColor c)
                          then "@" ++ n ++ ": " ++ c ++ ";"
                          else ""
    bfont :: String -> Maybe String -> String
    bfont _ Nothing = ""
    bfont n (Just c) = if (isValidFont c)
                          then "@" ++ n ++ ": " ++ c ++ ";"
                          else ""
