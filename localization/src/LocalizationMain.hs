module LocalizationMain(main) where

import KontraPrelude
import Text.StringTemplates.Templates hiding (runTemplatesT)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import User.Lang
import Templates

main :: IO ()
main = do
    putStr "Generating static localization templates..."
    templates <- readGlobalTemplates
    jsFileNameAndLocalizations <- forM allLangs $
      \lang -> runTemplatesT (lang, templates) $ do
        jsLocalized <- renderTemplate "javascriptLocalisation" $ F.value "code" $ codeFromLang lang
        return (codeFromLang lang ++ ".js", T.pack jsLocalized)
    createDirectoryIfMissing False "frontend/app/localization"
    forM_ jsFileNameAndLocalizations $ \(fn, text) -> T.writeFile ("frontend/app/localization/" ++ fn) text
    putStrLn "DONE"
