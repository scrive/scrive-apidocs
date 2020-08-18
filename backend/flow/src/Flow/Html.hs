{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.Html (
    CommonPageVars(..)
  , InstanceOverviewPageVars(..)
  , renderInstanceOverview
  , AuthErrorPageVars(..)
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

data CommonPageVars = CommonPageVars
  { production :: Bool
  , cdnBaseUrl :: Text
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

data AuthErrorPageVars = AuthErrorPageVars
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
    link ! rel "stylesheet" ! type_ "text/css" ! href (textValue signViewCssUrl)
    link ! rel "stylesheet" ! type_ "text/css" ! href (textValue brandingCssUrl)
  where
    signViewCssUrl = if production
      then cdnBaseUrl <> "/" <> versionCode <> ".signview-all-styling-minified.css"
      else "/less/signview-less-compiled.css"

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
  where crossorigin = attribute "crossorigin" "crossorigin=\""

renderAuthErrorPage :: AuthErrorPageVars -> Html
renderAuthErrorPage AuthErrorPageVars {..} = docTypeHtml $ do
  let CommonPageVars {..} = commonVars
  pageHeader commonVars
  body $ do
    div ! class_ "flow-overview signview" ! style "position: relative;" $ do
      logoHeader logoUrl
      div ! class_ "" $ do
        div ! class_ "main" $ do
          div ! class_ "section" $ do
            h4 $ toHtml explanation
      logoFooter cdnBaseUrl
