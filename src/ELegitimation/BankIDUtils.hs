module ELegitimation.BankIDUtils (
             mergeInfo
           , getTBS
           , getSigEntries
           , getSigEntry
           , fieldvaluebyid
           , compareFirstNames
           , normalizeNumber
           , compareNumbers
           , compareLastNames
           , compareSigLinkToElegData
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Functor

import Doc.DocStateData as D
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Utils.String

import Templates.Templates
import qualified Templates.Fields as F

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
                              -> m (Either (String, String, String, String) (Bool, Bool, Bool))
mergeInfo (contractFirst, contractLast, contractNumber) (elegFirst, elegLast, elegNumber) = do
  results <- sequence [ compareFirstNames contractFirst  elegFirst
                      , compareLastNames  contractLast   elegLast
                      , compareNumbers    contractNumber elegNumber]
  let failmsgs = [msg | MergeFail msg <- results]
      matches  = map (== MergeMatch) results
  if not $ null failmsgs
    then return $ Left  (intercalate ".\n " failmsgs, elegFirst, elegLast, elegNumber)
    else return $ Right (matches !! 0, matches !! 1, matches !! 2)

getTBS :: TemplatesMonad m => D.Document -> m String
getTBS doc = renderTemplate "tbs" $ do
  F.value "documentname"   $ documenttitle doc
  F.value "documentnumber" $ show $ documentid doc
  F.valueM "tbssigentries" $ getSigEntries doc

getSigEntries :: TemplatesMonad m => D.Document -> m String
getSigEntries doc = do
    s <- mapM (getSigEntry . signatorydetails) $ documentsignatorylinks doc
    return $ intercalate "\n" s

getSigEntry :: TemplatesMonad m => SignatoryDetails -> m String
getSigEntry signatorydetails =
    renderTemplate "tbssig" $ do
        F.value "firstname" $ getFirstName signatorydetails
        F.value "lastname"  $ getLastName signatorydetails
        F.value "company"   $ getCompanyName signatorydetails
        F.value "number"    $ getPersonalNumber signatorydetails

fieldvaluebyid :: String -> [(String, String)] -> String
fieldvaluebyid _ [] = ""
fieldvaluebyid fid ((k, v):xs)
    | k == fid  = v
    | otherwise = fieldvaluebyid fid xs

compareFirstNames :: TemplatesMonad m => String -> String -> m MergeResult
compareFirstNames fnContract fnEleg
    | null fnContract = do
      f <- renderTemplate "bankidNoFirstName" $ return ()
      return $ MergeFail f
    | null fnEleg = return MergeKeep
    | otherwise =
        let fnsc = words $ map toLower fnContract
            fnse = words $ map toLower fnEleg
            difs = [levenshtein a b | a <- fnsc, b <- fnse]
        in if any (<= 1) difs
            then return MergeMatch
            else do
             f <- renderTemplate "bankidFirstNameMismatch" $ do
               F.value "contract" fnContract
               F.value "eleg" fnEleg
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

compareLastNames :: TemplatesMonad m => String -> String -> m MergeResult
compareLastNames lnContract lnEleg
    | null lnContract = do
      f <- renderTemplate "bankidNoLastName" $ return ()
      return $ MergeFail f
    | null lnEleg = return MergeKeep
    | levenshtein (map toLower lnContract) (map toLower lnEleg) <= 1 = return MergeMatch
    | otherwise = do
      f <- renderTemplate "bankidLastNameMismatch" $ do
        F.value "contract" lnContract
        F.value "eleg" lnEleg
      return $ MergeFail f

--GHC.Unicode.toLower
-- import GHC.Unicode ( toLower )
--import qualified Data.ByteString.Lazy.Char8 as B


compareSigLinkToElegData :: TemplatesMonad m => SignatoryLink ->  [(FieldType, String)] -> [(String, String)] -> m (Either (String, String, String, String) (Bool, Bool, Bool))
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
