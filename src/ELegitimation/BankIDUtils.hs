module ELegitimation.BankIDUtils (
             mergeInfo
           , findTransactionByID
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
import Doc.DocStateData as D
import ELegitimation.ELegTransaction 
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.StringUtil

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
mergeInfo :: (String, String, String)
                -> (String, String, String)
                -> Either (String, String, String, String) (Bool, Bool, Bool)
mergeInfo (contractFirst, contractLast, contractNumber) (elegFirst, elegLast, elegNumber) =
    let results = [ compareFirstNames contractFirst  elegFirst
                  , compareLastNames  contractLast   elegLast
                  , compareNumbers    contractNumber elegNumber]
        failmsgs = [msg | MergeFail msg <- results]
        matches  = map (== MergeMatch) results
    in if not $ null failmsgs
        then Left  (intercalate "\n" failmsgs, elegFirst, elegLast, elegNumber)
        else Right (matches !! 0, matches !! 1, matches !! 2)

findTransactionByID :: String -> [ELegTransaction] -> Maybe ELegTransaction
findTransactionByID transactionsid = find ((==) transactionsid . transactiontransactionid)

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

compareFirstNames :: String -> String -> MergeResult
compareFirstNames fnContract fnEleg
    | null fnContract = MergeFail "Du har inte fyllt i förnamn, vänligen försök igen."
    | null fnEleg = MergeKeep
    | otherwise =
        let fnsc = words $ map toLower fnContract
            fnse = words $ map toLower fnEleg
            difs = [levenshtein a b | a <- fnsc, b <- fnse]
        in if any (<= 1) difs
            then MergeMatch
            else MergeFail $ "Förnamn matchar inte: '" ++ fnContract ++ "' och '" ++ fnEleg ++ "'."

normalizeNumber :: String -> String
normalizeNumber = filter isDigit

compareNumbers :: String -> String -> MergeResult
compareNumbers nContract nEleg
    | null nContract = MergeFail "Du har inte fyllt i personnnummer, vänligen försök igen."
    | null nEleg     = MergeKeep
    | otherwise =
        let nsc = normalizeNumber nContract
            nse = normalizeNumber nEleg
            dif = levenshtein nsc nse
        in if dif <= 3
            then MergeMatch
            else MergeFail $ "Personnnummer matchar inte: '" ++ nContract ++ "' och '" ++ nEleg ++ "'."

compareLastNames :: String -> String -> MergeResult
compareLastNames lnContract lnEleg
    | null lnContract = MergeFail "Du har inte fyllt i efternamn, vänligen försök igen."
    | null lnEleg = MergeKeep
    | levenshtein (map toLower lnContract) (map toLower lnEleg) <= 1 = MergeMatch
    | otherwise = MergeFail $ "Efternamn matchar inte: '" ++ lnContract ++ "' och '" ++ lnEleg ++"'."

--GHC.Unicode.toLower
-- import GHC.Unicode ( toLower )
--import qualified Data.ByteString.Lazy.Char8 as B


compareSigLinkToElegData :: SignatoryLink -> [(String, String)] -> Either (String, String, String, String) (Bool, Bool, Bool)
compareSigLinkToElegData sl attrs =
  -- compare information from document (and fields) to that obtained from BankID
  let contractFirst  = getFirstName sl
      contractLast   = getLastName sl
      contractNumber = getPersonalNumber sl
                
      elegFirst  = fieldvaluebyid "Subject.GivenName"    attrs
      elegLast   = fieldvaluebyid "Subject.Surname"      attrs
      elegNumber = fieldvaluebyid "Subject.SerialNumber" attrs

  in mergeInfo (contractFirst, contractLast, contractNumber)
               (elegFirst,     elegLast,     elegNumber)
