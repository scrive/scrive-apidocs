{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.CSS (
     signviewBrandingCSS
   , serviceBrandingCSS
   , loginBrandingCSS
   , scriveBrandingCSS
   , domainBrandingCSS
  ) where

import Control.Monad.Trans
import Log as Log
import System.Exit
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL

import BrandedDomain.BrandedDomain
import KontraPrelude
import Log.Utils
import Theme.Model
import Utils.Color
import Utils.Font

-- Signview branding CSS. Generated using less
signviewBrandingCSS :: (MonadLog m,MonadIO m) => Maybe String -> Theme -> m BSL.ByteString
signviewBrandingCSS cdnbaseurl theme = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ signviewBrandingLess cdnbaseurl theme)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          logAttention "Creating sign view branding failed" $ object [
              "stderr" `equalsExternalBSL` stderr
            ]
          return BSL.empty

signviewBrandingLess :: Maybe String -> Theme -> String
signviewBrandingLess cdnbaseurl theme = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions
    "@import 'runtime/signviewbranding/signviewbrandingdefaultvariables';" -- This will set default signview branding
      --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromTheme theme
     ++
   lessVariablesFromCDN cdnbaseurl
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/signviewbranding/signviewbranding';"
   ]

-- Service branding CSS. Generated using less
serviceBrandingCSS :: (MonadLog m,MonadIO m) => Maybe String -> Theme -> m BSL.ByteString
serviceBrandingCSS cdnbaseurl theme = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ serviceBrandingLess cdnbaseurl theme)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          logAttention "Creating service branding failed" $ object [
              "stderr" `equalsExternalBSL` stderr
            ]
          return BSL.empty

serviceBrandingLess :: Maybe String -> Theme -> String
serviceBrandingLess cdnbaseurl theme = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions
    "@import 'runtime/servicebranding/servicebrandingdefaultvariables';" -- This will set default signview branding
      --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromTheme theme
     ++
   lessVariablesFromCDN cdnbaseurl
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/servicebranding/servicebranding';"
   ]



-- Service branding CSS. Generated using less
loginBrandingCSS :: (MonadLog m,MonadIO m) => Maybe String -> Theme -> m BSL.ByteString
loginBrandingCSS cdnbaseurl theme = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ loginBrandingLess cdnbaseurl theme)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          logAttention "Creating login branding failed" $ object [
              "stderr" `equalsExternalBSL` stderr
            ]
          return BSL.empty

loginBrandingLess :: Maybe String -> Theme -> String
loginBrandingLess cdnbaseurl theme = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions

    "@import 'runtime/loginbranding/loginbrandingdefaultvariables';" -- This will set default signview branding
    --Following settings will overwrite default values
   ]
     ++
   lessVariablesFromTheme theme
     ++
   lessVariablesFromCDN cdnbaseurl
     ++
   [ -- Only last part will generate some css. Previews ones are just definitions
    "@import 'runtime/loginbranding/loginbranding';"
   ]

-- Scrive branding CSS. Generated using less. No DB involved, hence takes no `Theme`.
-- Should be used only for those pages that mimic the look of the company web ('Expression Engine').
scriveBrandingCSS :: (MonadLog m,MonadIO m) => Maybe String -> m BSL.ByteString
scriveBrandingCSS cdnbaseurl = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ scriveBrandingLess cdnbaseurl)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          logAttention "Creating Scrive branding failed" $ object [
              "stderr" `equalsExternalBSL` stderr
            ]
          return BSL.empty

scriveBrandingLess :: Maybe String -> String
scriveBrandingLess cdnbaseurl = unlines $
   [
    "@import 'branding/variables';", -- This is imported so we can use color variables from there
    "@import 'branding/elements';", -- This is imported so we can use some transform functions
    "@import 'runtime/scrivebranding/scrivebrandingdefaultvariables';",
    "@import 'runtime/scrivebranding/scrivebranding';"
   ] ++ lessVariablesFromCDN cdnbaseurl

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

lessVariablesFromCDN :: Maybe String -> [String]
lessVariablesFromCDN cdnbaseurl = [
    "@cdnbaseurl: \"" ++ (fromMaybe "" cdnbaseurl) ++ "\";"
  ]

domainBrandingCSS :: (MonadLog m,MonadIO m) => BrandedDomain -> m BSL.ByteString
domainBrandingCSS bd = do
    (code,stdout,stderr) <- liftIO $ do
      readProcessWithExitCode "lessc" ["--include-path=frontend/app/less" , "-" {-use stdin-} ]
        (BSL.fromString $ domainBrandingLess bd)
    case code of
      ExitSuccess -> do
          return $ stdout
      ExitFailure _ -> do
          logAttention "Creating domain branding failed" $ object [
              "stderr" `equalsExternalBSL` stderr
            ]
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
