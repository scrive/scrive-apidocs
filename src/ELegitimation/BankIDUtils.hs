module ELegitimation.BankIDUtils (
             mergeInfo
           , getTBS
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
import Util.HasSomeUserInfo
import Utils.String
import Text.StringTemplates.Templates
import Templates (renderLocalTemplate)
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
getTBS doc = renderLocalTemplate doc "tbs" $ do
  F.value "documentname"   $ documenttitle doc
  F.value "documentnumber" $ show $ documentid doc
  F.value "tbssigentries" $ unlines' $ map getSigEntry signatories
  where unlines' = intercalate "\n" -- like unlines, but without final newline
        signatories = filter signatoryispartner $ documentsignatorylinks doc
        getSigEntry signatory = unwords [ getFirstName signatory
                                        , getLastName signatory ++ ","
                                        , getPersonalNumber signatory
                                        ]

fieldvaluebyid :: String -> [(String, String)] -> String
fieldvaluebyid _ [] = ""
fieldvaluebyid fid ((k, v):xs)
    | k == fid  = v
    | otherwise = fieldvaluebyid fid xs

{- We want at least two words (First name and Last name) in name provided to Scrive to match any other two words in data from eleg.
   I guess this is best we can do.
-}
compareNames :: String -> String -> MergeResult
compareNames fnContractName fnElegName =
        let matchAnyElegData w = any (\x -> maxLev w x 2 ) $ words $ map toLower fnElegName
            matches = length $ filter matchAnyElegData $ words $ map toLower fnContractName
        in if matches >= 2
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

