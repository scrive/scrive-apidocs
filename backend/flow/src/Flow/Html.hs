{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.Html (
    InstanceOverviewTemplateVars(..)
  , renderInstanceOverview
  , AuthErrorTemplateVars(..)
  , renderAuthErrorPage
  ) where

import Data.Text as T
import Servant (toUrlPiece)
import Text.RawString.QQ

import Flow.Id

data InstanceOverviewTemplateVars = InstanceOverviewTemplateVars
  { cdnBaseUrl :: Text
  , versionCode :: Text
  , kontraApiUrl :: Text
  , flowApiUrl :: Text
  , flowInstanceId :: InstanceId
  }

renderInstanceOverview :: InstanceOverviewTemplateVars -> Text
renderInstanceOverview InstanceOverviewTemplateVars {..} =
  T.replace "$cdnBaseUrl$" cdnBaseUrl
    . T.replace "$versionCode$" versionCode
    . T.replace "$kontraApiUrl$" kontraApiUrl
    . T.replace "$flowApiUrl$" flowApiUrl
    . T.replace "$flowInstanceId$" (toUrlPiece flowInstanceId)
    $ instanceOverviewTemplate

-- TODO: Use a proper templating library like the one in kontrakcja.
instanceOverviewTemplate :: Text
instanceOverviewTemplate = [r|
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Scrive Flow Overview</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, maximum-scale=1, initial-scale=1, user-scalable=no" />
    <meta name="format-detection" content="telephone=no"/>
    <meta name="robots" content="noindex">
    <link rel="stylesheet" type="text/css" href="$cdnBaseUrl$/elm-assets/flow-overview-$versionCode$.css"/>
    <link rel="stylesheet" type="text/css" href="/less/signview-less-compiled.css"/>
    <!-- TODO: Branding -->
    <link rel="stylesheet" type="text/css" href="$cdnBaseUrl$/elm-assets/flow-dummy-branding.css"/>
  </head>
  <body>

  <div class="flow-overview signview" style="position: relative;">
    <div class="header">
      <div class="main">
          <div class="col-xs-12 left vertical">
              <div class="middle" >
                <!-- TODO: Branding, dynamic image: /signview_logo/1/12/ca98172b -->
                <img class="logo" src="$cdnBaseUrl$/elm-assets/flow-images/scrive-logo.png" alt="Scrive">
              </div>
          </div>
      </div>
    </div>

    <div id="elm-mount" class="col-xs-12 loading">
      Loading...
    </div>

    <div class="main" >
      <div class="section footer">
        <div class="col-xs-12">
          <img class="logo" src="$cdnBaseUrl$/elm-assets/flow-images/poweredby.svg" alt="Powered by Scrive">
        </div>
      </div>
    </div>
  </div>

  <script>
    var elmFlagsFromTemplate = {
      cookie: document.cookie || '',
      kontraApiUrl: "$kontraApiUrl$",
      flowApiUrl: "$flowApiUrl$",
      flowInstanceId: "$flowInstanceId$"
    };
  </script>
  <script src="$cdnBaseUrl$/elm-assets/flow-overview-$versionCode$.js" crossorigin="anonymous"></script>

  </body>
</html>
|]

data AuthErrorTemplateVars = AuthErrorTemplateVars
  { explanation :: Text
  , cdnBaseUrl :: Text
  , versionCode :: Text
  , kontraApiUrl :: Text
  , flowApiUrl :: Text
  }

renderAuthErrorPage :: AuthErrorTemplateVars -> Text
renderAuthErrorPage AuthErrorTemplateVars {..} =
  T.replace "$explanation$" explanation
    . T.replace "$cdnBaseUrl$" cdnBaseUrl
    . T.replace "$versionCode$" versionCode
    . T.replace "$kontraApiUrl$" kontraApiUrl
    . T.replace "$flowApiUrl$" flowApiUrl
    $ authErrorPageTemplate

-- TODO: Use a proper templating library like the one in kontrakcja.
authErrorPageTemplate :: Text
authErrorPageTemplate = [r|
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Scrive Flow Authorisation Error</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, maximum-scale=1, initial-scale=1, user-scalable=no" />
    <meta name="format-detection" content="telephone=no"/>
    <meta name="robots" content="noindex">
    <link rel="stylesheet" type="text/css" href="$cdnBaseUrl$/elm-assets/flow-overview-$versionCode$.css"/>
    <link rel="stylesheet" type="text/css" href="/less/signview-less-compiled.css"/>
    <!-- TODO: Branding -->
    <link rel="stylesheet" type="text/css" href="$cdnBaseUrl$/elm-assets/flow-dummy-branding.css"/>
  </head>
  <body>

  <div class="flow-overview signview" style="position: relative;">
    <div class="header">
      <div class="main">
          <div class="col-xs-12 left vertical">
              <div class="middle" >
                <!-- TODO: Branding, dynamic image: /signview_logo/1/12/ca98172b -->
                <img class="logo" src="$cdnBaseUrl$/elm-assets/flow-images/scrive-logo.png" alt="Scrive">
              </div>
          </div>
      </div>
    </div>

    <div class="">
      <div class="main">
        <div class="section">
          <h4>$explanation$</h4>
        </div>
      </div>
    </div>

    <div class="main" >
      <div class="section footer">
        <div class="col-xs-12">
          <img class="logo" src="$cdnBaseUrl$/elm-assets/flow-images/poweredby.svg" alt="Powered by Scrive">
        </div>
      </div>
    </div>
  </div>

  </body>
</html>
|]
