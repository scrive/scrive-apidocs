{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.CSS (
     signviewBrandingCSS
   , serviceBrandingCSS
   , loginBrandingCSS
   , domainBrandingCSS
  ) where

import Control.Monad.Trans
import System.Exit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL

import BrandedDomain.BrandedDomain
import Theme.Model
import Utils.Color
import Utils.Font
import Utils.IO
import qualified Log as Log

-- Signview branding CSS. Generated using less
signviewBrandingCSS :: (Log.MonadLog m,MonadIO m) => Theme -> m BSL.ByteString
signviewBrandingCSS theme = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode' "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ signviewBrandingLess theme)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          Log.attention_ $ "Creating sign view branding failed : " ++ BSL.toString stderr
          return BSL.empty


signviewBrandingLess :: Theme -> String
signviewBrandingLess theme = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions
    "@import 'runtime/signviewbranding/signviewbrandingdefaultvariables';" -- This will set default signview branding
      --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromTheme theme
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/signviewbranding/signviewbranding';"
   ]

-- Service branding CSS. Generated using less
serviceBrandingCSS :: (Log.MonadLog m,MonadIO m) => Theme -> m BSL.ByteString
serviceBrandingCSS theme = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode' "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ serviceBrandingLess theme)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          Log.attention_ $ "Creating service branding failed : " ++ BSL.toString stderr
          return BSL.empty

serviceBrandingLess :: Theme -> String
serviceBrandingLess theme = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions
    "@import 'runtime/servicebranding/servicebrandingdefaultvariables';" -- This will set default signview branding
      --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromTheme theme
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/servicebranding/servicebranding';"
   ]



-- Service branding CSS. Generated using less
loginBrandingCSS :: (Log.MonadLog m,MonadIO m) => Theme -> m BSL.ByteString
loginBrandingCSS theme = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode' "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ loginBrandingLess theme)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          Log.attention_ $ "Creating login branding failed : " ++ BSL.toString stderr
          return BSL.empty

loginBrandingLess :: Theme -> String
loginBrandingLess theme = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions

    "@import 'runtime/loginbranding/loginbrandingdefaultvariables';" -- This will set default signview branding
    --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromTheme theme
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/loginbranding/loginbranding';"
   ]

lessVariablesFromTheme :: Theme -> [String]
lessVariablesFromTheme theme = [
    bcolor "brandcolor" $ themeBrandColor theme,
    bcolor "brandtextcolor" $ themeBrandTextColor theme,
    bcolor "actioncolor" $ themeActionColor theme,
    bcolor "actiontextcolor" $ themeActionTextColor theme,
    bcolor "actionsecondarycolor" $ themeActionSecondaryColor theme,
    bcolor "actionsecondarytextcolor" $ themeActionSecondaryTextColor theme,
    bcolor "positivecolor" $ themePositiveColor theme,
    bcolor "positivetextcolor" $ themePositiveTextColor theme,
    bcolor "negativecolor" $ themeNegativeColor theme,
    bcolor "negativetextcolor" $ themeNegativeTextColor theme,
    bfont "font" $ themeFont theme
  ]
  where

domainBrandingCSS :: (Log.MonadLog m,MonadIO m) => BrandedDomain -> m BSL.ByteString
domainBrandingCSS bd = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode' "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ domainBrandingLess bd)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          Log.attention_ $ "Creating domain branding failed : " ++ BSL.toString stderr
          return BSL.empty

domainBrandingLess :: BrandedDomain -> String
domainBrandingLess bd = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions

    "@import 'runtime/domainbranding/domainbrandingdefaultvariables';" -- This will set default signview branding
    --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromDomain bd
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/domainbranding/domainbranding';"
   ]

lessVariablesFromDomain :: BrandedDomain -> [String]
lessVariablesFromDomain bd = [
    bcolor "participantcolor1" $  bdParticipantColor1 bd,
    bcolor "participantcolor2" $  bdParticipantColor2 bd,
    bcolor "participantcolor3" $  bdParticipantColor3 bd,
    bcolor "participantcolor4" $  bdParticipantColor4 bd,
    bcolor "participantcolor5" $  bdParticipantColor5 bd,
    bcolor "participantcolor6" $  bdParticipantColor6 bd,
    bcolor "draftcolor" $ bdDraftColor bd,
    bcolor "cancelledcolor" $ bdCancelledColor bd,
    bcolor "initiatedcolor" $ bdInitatedColor bd,
    bcolor "sentcolor" $ bdSentColor bd,
    bcolor "deliveredcolor" $ bdDeliveredColor bd,
    bcolor "openedcolor" $ bdOpenedColor bd,
    bcolor "reviewedcolor" $ bdReviewedColor bd,
    bcolor "signedcolor" $ bdSignedColor bd
  ]

-- Some sanity checks on data. Note that this are provided by users
bcolor :: String -> String -> String
bcolor n c = if (isValidColor c)
                          then "@" ++ n ++ ": " ++ c ++ ";"
                          else ""

bfont :: String -> String -> String
bfont n c = if (isValidFont c)
                          then "@" ++ n ++ ": " ++ c ++ ";"
                          else ""
