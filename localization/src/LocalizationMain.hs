module LocalizationMain(main) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.StringTemplates.Templates hiding (runTemplatesT)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Text.StringTemplates.Fields as F

import AppDir (AppPaths(..), setupAppPaths)
import Templates
import User.Lang
import Version

main :: IO ()
main = do
  putStrLn "Generating static localization templates..."
  (AppPaths sourceRoot _) <- setupAppPaths

  templates               <- readGlobalTemplatesFrom (sourceRoot </> textsDirectory)
                                                     (sourceRoot </> templateFilesDir)

  versionID <- genVersionID
  let versionIDHex = TE.decodeUtf8 . B16.encode . BS.fromString $ versionID
  jsFileNameAndLocalizations <- forM allLangs $ \lang ->
    runTemplatesT (lang, templates) $ do
      jsLocalized <-
        renderTemplate "javascriptLocalisation" . F.value "code" $ codeFromLang lang
      return (versionIDHex <> "." <> codeFromLang lang <> ".js", T.pack jsLocalized)
  createDirectoryIfMissing False (sourceRoot </> "frontend/app/localization")
  forM_ jsFileNameAndLocalizations $ \(fn, text) ->
    T.writeFile (sourceRoot </> "frontend/app/localization" </> T.unpack fn) text
  putStrLn "DONE"
