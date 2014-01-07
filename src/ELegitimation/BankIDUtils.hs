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
           , getDetailsFromResponseAttrs
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
                 | MergeFail
     deriving (Eq, Show)

{- | Compare signatory information from contract with that from the
     E-Legitimation provider. Returns Either and error message or the
     correct value.
 -}
mergeInfo :: (String, String, String) -> (String, String, String) -> (MergeResult,MergeResult)
mergeInfo (contractFirst, contractLast, contractNumber) (elegFirst, elegLast, elegNumber) = do
  (compareNames (contractFirst ++ " "++ contractLast)  (elegFirst ++ " "++ elegLast), compareNumbers contractNumber elegNumber)


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


compareNames :: String -> String -> MergeResult
compareNames fnContractName fnElegName =
        let fnsc = words $ map toLower fnContractName
            fnse = words $ map toLower fnElegName
            difs = [maxLev a b 2 | a <- fnsc, b <- fnse]
        in if any (== True) difs
            then MergeMatch
            else MergeFail


normalizeNumber :: String -> String
normalizeNumber = filter isDigit

compareNumbers :: String -> String -> MergeResult
compareNumbers nContract nEleg
    | null nContract = MergeFail
    | null nEleg     = MergeKeep
    | otherwise =
        let nsc = normalizeNumber nContract
            nse = normalizeNumber nEleg
        in if (maxLev nsc nse 3)
            then MergeMatch
            else MergeFail

--GHC.Unicode.toLower
-- import GHC.Unicode ( toLower )
--import qualified Data.ByteString.Lazy.Char8 as B


getDetailsFromResponseAttrs :: [(String, String)] -> (String,String,String)
getDetailsFromResponseAttrs attrs =
  let
      elegFirst  = fieldvaluebyid "Subject.GivenName"    attrs
      elegLast   = fieldvaluebyid "Subject.Surname"      attrs
      elegNumber = fieldvaluebyid "Subject.SerialNumber" attrs
  in (elegFirst,     elegLast,     elegNumber)

compareSigLinkToElegData :: SignatoryLink ->  [(FieldType, String)] -> [(String, String)] -> (MergeResult,MergeResult)
compareSigLinkToElegData sl fields attrs =
  -- compare information from document (and fields) to that obtained from BankID
  let contractFirst  = fromMaybe (getFirstName sl)      (snd <$> (find (\(ft,_) -> ft == FirstNameFT) fields))
      contractLast   = fromMaybe (getLastName sl)       (snd <$> (find (\(ft,_) -> ft == LastNameFT) fields))
      contractNumber = fromMaybe (getPersonalNumber sl) (snd <$> (find (\(ft,_) -> ft == PersonalNumberFT) fields))

  in mergeInfo (contractFirst, contractLast, contractNumber) (getDetailsFromResponseAttrs attrs)

