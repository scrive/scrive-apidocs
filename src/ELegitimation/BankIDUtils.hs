module ELegitimation.BankIDUtils (
             mergeInfo
           , getTBS
           , getSigEntries
           , getSigEntry
           , fieldvaluebyid
           , normalizeNumber
           , compareNumbers
           , compareNames
           , compareSigLinkToElegData
           , MergeResult(..)
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Functor

import Doc.DocStateData as D
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Utils.String
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

data MergeResult = MergeMatch
                 | MergeKeep
                 | MergeFail String
     deriving (Eq, Show)

{- | Compare signatory information from contract with that from the
     E-Legitimation provider. Returns Either and error message or the
     correct value.
 -}
mergeInfo :: TemplatesMonad m => (String, String, String)
                              -> (String, String, String)
                              -> m (Either (String, String, String, String) (Bool, Bool))
mergeInfo (contractFirst, contractLast, contractNumber) (elegFirst, elegLast, elegNumber) = do
  results <- sequence [ compareNames (contractFirst ++ " "++ contractLast)  (elegFirst ++ " "++ elegLast)
                      , compareNumbers    contractNumber elegNumber]
  let failmsgs = [msg | MergeFail msg <- results]
      matches  = map (== MergeMatch) results
  if not $ null failmsgs
    then return $ Left  (intercalate ".\n " failmsgs, elegFirst, elegLast, elegNumber)
    else return $ Right (matches !! 0, matches !! 1)

getTBS :: TemplatesMonad m => D.Document -> m String
getTBS doc = renderTemplate "tbs" $ do
  F.value "documentname"   $ documenttitle doc
  F.value "documentnumber" $ show $ documentid doc
  F.valueM "tbssigentries" $ getSigEntries doc

getSigEntries :: TemplatesMonad m => D.Document -> m String
getSigEntries doc = do
    s <- mapM getSigEntry $ filter signatoryispartner $ documentsignatorylinks doc
    return $ intercalate "\n" s

getSigEntry :: TemplatesMonad m => SignatoryLink -> m String
getSigEntry signatory =
    renderTemplate "tbssig" $ do
        F.value "firstname" $ getFirstName signatory
        F.value "lastname"  $ getLastName signatory
        F.value "company"   $ getCompanyName signatory
        F.value "number"    $ getPersonalNumber signatory

fieldvaluebyid :: String -> [(String, String)] -> String
fieldvaluebyid _ [] = ""
fieldvaluebyid fid ((k, v):xs)
    | k == fid  = v
    | otherwise = fieldvaluebyid fid xs


compareNames :: TemplatesMonad m => String -> String -> m MergeResult
compareNames fnContractName fnElegName =
        let fnsc = words $ map toLower fnContractName
            fnse = words $ map toLower fnElegName
            difs = [levenshtein a b | a <- fnsc, b <- fnse]
        in if any (<= 2) difs
            then return MergeMatch
            else do
             f <- renderTemplate "bankidNameMismatch" $ do
               F.value "name" fnContractName
               F.value "eleg" fnElegName
             return $ MergeFail f


normalizeNumber :: String -> String
normalizeNumber = filter isDigit

compareNumbers :: TemplatesMonad m => String -> String -> m MergeResult
compareNumbers nContract nEleg
    | null nContract = do
      f <- renderTemplate "bankidNoNumber" $ return ()
      return $ MergeFail f
    | null nEleg     = return MergeKeep
    | otherwise =
        let nsc = normalizeNumber nContract
            nse = normalizeNumber nEleg
            dif = levenshtein nsc nse
        in if dif <= 3
            then return MergeMatch
            else do
             f <- renderTemplate "bankidNumberMismatch" $ do
               F.value "contract" nContract
               F.value "eleg" nEleg
             return $ MergeFail f

--GHC.Unicode.toLower
-- import GHC.Unicode ( toLower )
--import qualified Data.ByteString.Lazy.Char8 as B


compareSigLinkToElegData :: TemplatesMonad m => SignatoryLink ->  [(FieldType, String)] -> [(String, String)] -> m (Either (String, String, String, String) (Bool, Bool))
compareSigLinkToElegData sl fields attrs =
  -- compare information from document (and fields) to that obtained from BankID
  let contractFirst  = fromMaybe (getFirstName sl)      (snd <$> (find (\(ft,_) -> ft == FirstNameFT) fields))
      contractLast   = fromMaybe (getLastName sl)       (snd <$> (find (\(ft,_) -> ft == LastNameFT) fields))
      contractNumber = fromMaybe (getPersonalNumber sl) (snd <$> (find (\(ft,_) -> ft == PersonalNumberFT) fields))

      elegFirst  = fieldvaluebyid "Subject.GivenName"    attrs
      elegLast   = fieldvaluebyid "Subject.Surname"      attrs
      elegNumber = fieldvaluebyid "Subject.SerialNumber" attrs

  in mergeInfo (contractFirst, contractLast, contractNumber)
               (elegFirst,     elegLast,     elegNumber)
