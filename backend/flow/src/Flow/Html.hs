{-# LANGUAGE QuasiQuotes #-}
module Flow.Html (
    CommonPageVars(..)
  , InstanceOverviewPageVars(..)
  , IdentifyViewVars(..)
  , IdentifyViewAppConfig(..)
  , ErrorPageVars(..)
  , renderInstanceOverview
  , renderIdentifyView
  , renderErrorPage
  , mkErrorPageVars
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Prelude hiding (div, head)
import Servant (toUrlPiece)
import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.RawString.QQ
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import BrandedDomain.Model
import Branding.Adler32
import DB
import Doc.Types.SignatoryLink (AuthenticationToViewMethod)
import Flow.Id
import Flow.Server.Types
import VersionTH

data CommonPageVars = CommonPageVars
  { cdnBaseUrl :: Text
  , mainCssUrl :: Text
  , brandingCssUrl :: Text
  , logoUrl :: Text
  , versionCode :: Text
  , browserTitle :: Text
  }

data InstanceOverviewPageVars = InstanceOverviewPageVars
  { commonVars :: CommonPageVars
  , kontraApiUrl :: Text
  , flowApiUrl :: Text
  , flowInstanceId :: InstanceId
  }

data IdentifyViewVars = IdentifyViewVars
  { commonVars :: CommonPageVars
  , appConfig :: IdentifyViewAppConfig
  }

data IdentifyViewAppConfig = IdentifyViewAppConfig
  { cdnBaseUrl :: Text
  , logoUrl :: Text
  , welcomeText :: Text
  , entityTypeLabel :: Text
  , entityTitle :: Text
  , authenticationMethod :: AuthenticationToViewMethod
  , authorName :: Text
  , participantEmail :: Text
  , participantMaskedMobile :: Text
  , genericEidServiceStartUrl :: Maybe Text
  , smsPinSendUrl :: Text
  , smsPinVerifyUrl :: Text
  , rejectionRejectUrl :: Text
  , rejectionAlreadyRejected :: Bool
  , errorMessage :: Maybe Text
  , maxFailuresExceeded :: Bool
  } deriving (Generic)

instance ToJSON IdentifyViewAppConfig where
  toEncoding = genericToEncoding defaultOptions

data ErrorPageVars = ErrorPageVars
  { commonVars :: CommonPageVars
  , explanation :: Text
  }

pageHeader :: CommonPageVars -> Html
pageHeader CommonPageVars {..} = do
  head $ do
    title $ toHtml browserTitle
    meta ! charset "utf-8"
    meta ! name "viewport" ! content
      "width=device-width, maximum-scale=1, initial-scale=1, user-scalable=no"
    meta ! name "format-detection" ! content "telephone=no"
    meta ! name "robots" ! content "noindex"
    link ! rel "stylesheet" ! type_ "text/css" ! href
      (textValue $ cdnBaseUrl <> "/elm-assets/flow-overview-" <> versionCode <> ".css")
    link ! rel "stylesheet" ! type_ "text/css" ! href (textValue mainCssUrl)
    link ! rel "stylesheet" ! type_ "text/css" ! href (textValue brandingCssUrl)
    -- TODO: Real localisation. At the moment we just fetch English texts for identify view
    script
      ! src (textValue $ cdnBaseUrl <> "/localization/" <> versionCode <> ".en.js")
      ! crossorigin "anonymous"
      $ pure ()

logoHeader :: Text -> Html
logoHeader logoUrl = do
  div ! class_ "header" $ do
    div ! class_ "main" $ do
      div ! class_ "col-xs-12 left vertical" $ do
        div ! class_ "middle" $ do
          img ! class_ "logo" ! src (textValue logoUrl) ! alt "Logo"

logoFooter :: Text -> Html
logoFooter cdnBaseUrl = do
  div ! class_ "main" $ do
    div ! class_ "section footer" $ do
      div ! class_ "col-xs-12" $ do
        img
          ! class_ "logo"
          ! src (textValue $ cdnBaseUrl <> "/elm-assets/flow-images/poweredby.svg")
          ! alt "Powered by Scrive"

renderInstanceOverview :: InstanceOverviewPageVars -> Html
renderInstanceOverview InstanceOverviewPageVars {..} = docTypeHtml $ do
  let CommonPageVars {..} = commonVars
  pageHeader commonVars
  body $ do
    div ! class_ "flow-overview signview" ! style "position: relative;" $ do
      logoHeader logoUrl
      div ! class_ "col-xs-12 loading" ! id "elm-mount" $ "Loading ..."
      logoFooter cdnBaseUrl
      script
        . toHtml
        . T.replace "$kontraApiUrl$" kontraApiUrl
        . T.replace "$flowApiUrl$" flowApiUrl
        . T.replace "$flowInstanceId$" (toUrlPiece flowInstanceId)
        $ [r|
              var elmFlagsFromTemplate = {
                cookie: document.cookie || '',
                kontraApiUrl: "$kontraApiUrl$",
                flowApiUrl: "$flowApiUrl$",
                flowInstanceId: "$flowInstanceId$"
              };
            |]
      script
        ! src
            (  textValue
            $  cdnBaseUrl
            <> "/elm-assets/flow-overview-"
            <> versionCode
            <> ".js"
            )
        ! crossorigin "anonymous"
        $ pure ()

renderIdentifyView :: IdentifyViewVars -> Html
renderIdentifyView IdentifyViewVars {..} = docTypeHtml $ do
  let CommonPageVars {..} = commonVars
  pageHeader commonVars
  body $ do
    div ! class_ "global-table" $ do
      div ! class_ "global-table-cell" $ do
        div ! id "elm-mount" $ ""
    script
      . toHtml
      . T.replace "$appConfig$" (LT.toStrict . LT.decodeUtf8 $ encode appConfig)
      $ [r| var appConfigFromTemplate = $appConfig$; |]
    script
      ! src
          (textValue $ cdnBaseUrl <> "/elm-assets/identifyview-" <> versionCode <> ".js")
      ! crossorigin "anonymous"
      $ pure ()

crossorigin :: AttributeValue -> Attribute
crossorigin = customAttribute "crossorigin"

renderErrorPage :: ErrorPageVars -> Html
renderErrorPage ErrorPageVars {..} = docTypeHtml $ do
  let CommonPageVars {..} = commonVars
  pageHeader commonVars
  body $ do
    div ! class_ "wrapper-position-footer" $ do
      div ! class_ "mainContainer big error-page" $ do
        div ! class_ "error-page-header" $ do
          div ! class_ "error-page-header-logo-wrapper" $ do
            img ! class_ "logo" ! src (textValue logoUrl) ! alt "Logo"
            div ! class_ "divider-line" $ ""
            div ! class_ "label" $ "E-signing powered by Scrive"
        div ! class_ "error-page-info" $ do
          div ! class_ "error-page-description" $ do
            h1 $ toHtml browserTitle
            h4 $ toHtml explanation
          div ! class_ "error-page-instruction" $ "Visit any of these pages instead"
          div ! class_ "error-page-buttons-box" $ do
            a ! class_ "button main float-left" ! href "/" $ "Homepage"
            a
              ! class_ "button main float-right"
              ! href "#"
              ! onclick "history.go(-1);return false;"
              $ "Back"

mkErrorPageVars
  :: (MonadReader FlowContext m, MonadDB m, MonadThrow m)
  => BrandedDomain
  -> Text
  -> Text
  -> m ErrorPageVars
mkErrorPageVars bd browserTitle explanation = do
  FlowContext { cdnBaseUrl, production } <- ask
  brandingHash <- brandingAdler32 bd Nothing Nothing

  let
    cdnBaseUrl'    = fromMaybe "" cdnBaseUrl

    brandingCssUrl = T.intercalate
      "/"
      [cdnBaseUrl', "login_branding", showt (bd ^. #id), brandingHash <> "-styles.css"]

    logoUrl =
      T.intercalate "/" [cdnBaseUrl', "login_logo", showt (bd ^. #id), brandingHash]

    mainCssUrl = if production
      then cdnBaseUrl' <> "/" <> versionCode <> ".all-styling-minified.css"
      else "/less/less-compiled.css"

    versionCode = T.decodeUtf8 . B16.encode $ BS.fromString versionID

  pure $ ErrorPageVars
    { commonVars  = CommonPageVars { cdnBaseUrl     = cdnBaseUrl'
                                   , mainCssUrl     = mainCssUrl
                                   , brandingCssUrl = brandingCssUrl
                                   , logoUrl        = logoUrl
                                   , versionCode    = versionCode
                                   , browserTitle   = browserTitle
                                   }
    , explanation = explanation
    }
