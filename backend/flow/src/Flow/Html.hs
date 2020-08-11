{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.Html (
    InstanceOverviewTemplateVars(..)
  , renderInstanceOverview
  , AuthErrorTemplateVars(..)
  , renderAuthErrorPage
  ) where

import Data.Text as T hiding (head)
import Prelude hiding (div, head)
import Servant (toUrlPiece)
import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Internal (attribute)
import Text.RawString.QQ

import Flow.Id

pageHeader :: Bool -> Text -> Text -> Text -> Html
pageHeader production cdnBaseUrl versionCode titleText = do
  head $ do
    title $ toHtml titleText
    meta ! charset "utf-8"
    meta ! name "viewport" ! content
      "width=device-width, maximum-scale=1, initial-scale=1, user-scalable=no"
    meta ! name "format-detection" ! content "telephone=no"
    meta ! name "robots" ! content "noindex"
    link ! rel "stylesheet" ! type_ "text/css" ! href
      (textValue $ cdnBaseUrl <> "/elm-assets/flow-overview-" <> versionCode <> ".css")
    link ! rel "stylesheet" ! type_ "text/css" ! href (textValue signviewCssUrl)
    link ! rel "stylesheet" ! type_ "text/css" ! href
      (textValue $ cdnBaseUrl <> "/elm-assets/flow-dummy-branding.css")
  where
    signviewCssUrl = if production
      then cdnBaseUrl <> "/" <> versionCode <> ".signview-all-styling-minified.css"
      else "/less/signview-less-compiled.css"

logoHeader :: Text -> Html
logoHeader cdnBaseUrl = do
  div ! class_ "header" $ do
    div ! class_ "main" $ do
      div ! class_ "col-xs-12 left vertical" $ do
        div ! class_ "middle" $ do
          img
            ! class_ "logo"
            ! src (textValue $ cdnBaseUrl <> "/elm-assets/flow-images/scrive-logo.png")
            ! alt "Scrive"

logoFooter :: Text -> Html
logoFooter cdnBaseUrl = do
  div ! class_ "main" $ do
    div ! class_ "section footer" $ do
      div ! class_ "col-xs-12" $ do
        img
          ! class_ "logo"
          ! src (textValue $ cdnBaseUrl <> "/elm-assets/flow-images/poweredby.svg")
          ! alt "Powered by Scrive"

data InstanceOverviewTemplateVars = InstanceOverviewTemplateVars
  { production :: Bool
  , cdnBaseUrl :: Text
  , versionCode :: Text
  , kontraApiUrl :: Text
  , flowApiUrl :: Text
  , flowInstanceId :: InstanceId
  }

renderInstanceOverview :: InstanceOverviewTemplateVars -> Html
renderInstanceOverview InstanceOverviewTemplateVars {..} = docTypeHtml $ do
  pageHeader production cdnBaseUrl versionCode "Scrive Flow Overview"
  body $ do
    div ! class_ "flow-overview signview" ! style "position: relative;" $ do
      logoHeader cdnBaseUrl
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
  where crossorigin = attribute "crossorigin" "crossorigin=\""

data AuthErrorTemplateVars = AuthErrorTemplateVars
  { production :: Bool
  , cdnBaseUrl :: Text
  , versionCode :: Text
  , explanation :: Text
  }

renderAuthErrorPage :: AuthErrorTemplateVars -> Html
renderAuthErrorPage AuthErrorTemplateVars {..} = docTypeHtml $ do
  pageHeader production cdnBaseUrl versionCode "Scrive Flow Authorisation Error"
  body $ do
    div ! class_ "flow-overview signview" ! style "position: relative;" $ do
      logoHeader cdnBaseUrl
      div ! class_ "" $ do
        div ! class_ "main" $ do
          div ! class_ "section" $ do
            h4 $ toHtml explanation
      logoFooter cdnBaseUrl
