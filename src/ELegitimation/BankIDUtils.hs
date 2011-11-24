module ELegitimation.BankIDUtils (
             mergeInfo
           , findTransactionByIDOrFail
           , getTBS
           , getSigEntries
           , getSigEntry
           , fieldvaluebyid
           , compareFirstNames
           , bsdigits
           , isBSDigit
           , normalizeNumber
           , compareNumbers
           , compareLastNames
           , compareSigLinkToElegData
    ) where

import Data.List
import Doc.Transitory as D
import ELegitimation.ELegTransaction 
import Kontra
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop, break)
import GHC.Word
import GHC.Unicode (toLower)
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.StringUtil

import Templates.Templates



data MergeResult = MergeMatch
                 | MergeKeep
                 | MergeFail String
     deriving (Eq, Show)

{- | Compare signatory information from contract with that from the
     E-Legitimation provider. Returns Either and error message or the
     correct value.
 -}
mergeInfo :: (BS.ByteString, BS.ByteString, BS.ByteString)
                -> (BS.ByteString, BS.ByteString, BS.ByteString)
                -> Either (String, BS.ByteString, BS.ByteString, BS.ByteString) (Bool, Bool, Bool)
mergeInfo (contractFirst, contractLast, contractNumber) (elegFirst, elegLast, elegNumber) =
    let results = [ compareFirstNames contractFirst  elegFirst
                  , compareLastNames  contractLast   elegLast
                  , compareNumbers    contractNumber elegNumber]
        failmsgs = [msg | MergeFail msg <- results]
        matches  = map (== MergeMatch) results
    in if not $ null failmsgs
        then Left  (intercalate "\n" failmsgs, elegFirst, elegLast, elegNumber)
        else Right (matches !! 0, matches !! 1, matches !! 2)

findTransactionByIDOrFail :: Kontrakcja m => [ELegTransaction] -> String -> m ELegTransaction
findTransactionByIDOrFail transactions transactionsid =
    guardJust $ find ((==) transactionsid . transactiontransactionid) transactions

getTBS :: TemplatesMonad m => D.Document -> m String
getTBS doc = do
    entries <- getSigEntries doc
    renderTemplateFM "tbs" $ do
        field "documentname"   $ documenttitle doc
        field "documentnumber" $ show $ documentid doc
        field "tbssigentries"  entries

getSigEntries :: TemplatesMonad m => D.Document -> m String
getSigEntries doc = do
    s <- mapM (getSigEntry . signatorydetails) $ documentsignatorylinks doc
    return $ intercalate "\n" s

getSigEntry :: TemplatesMonad m => SignatoryDetails -> m String
getSigEntry signatorydetails =
    renderTemplateFM "tbssig" $ do
        field "firstname" $ getFirstName signatorydetails
        field "lastname"  $ getLastName signatorydetails
        field "company"   $ getCompanyName signatorydetails
        field "number"    $ getPersonalNumber signatorydetails

fieldvaluebyid :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
fieldvaluebyid _ [] = BS.fromString ""
fieldvaluebyid fid ((k, v):xs)
    | k == fid  = v
    | otherwise = fieldvaluebyid fid xs

compareFirstNames :: BS.ByteString -> BS.ByteString -> MergeResult
compareFirstNames fnContract fnEleg
    | BS.null fnContract = MergeFail "Du har inte fyllt i förnamn, vänligen försök igen."
    | BS.null fnEleg = MergeKeep
    | otherwise =
        let fnsc = words $ map toLower $ BS.toString fnContract
            fnse = words $ map toLower $ BS.toString fnEleg
            difs = [levenshtein a b | a <- fnsc, b <- fnse]
        in if any (<= 1) difs
            then MergeMatch
            else MergeFail $ "Förnamn matchar inte: \"" ++ BS.toString fnContract ++ "\" och \"" ++ BS.toString fnEleg ++ "\"."

bsdigits :: BS.ByteString
bsdigits = BS.fromString "0123456789"

isBSDigit :: Word8 -> Bool
isBSDigit x = x `BS.elem` bsdigits

normalizeNumber :: BS.ByteString -> BS.ByteString
normalizeNumber n | BS.null n = n
normalizeNumber n
    | isBSDigit (BS.head n) = BS.cons (BS.head n) $ normalizeNumber (BS.tail n)
    | otherwise = normalizeNumber (BS.tail n)

compareNumbers :: BS.ByteString -> BS.ByteString -> MergeResult
compareNumbers nContract nEleg
    | BS.null nContract = MergeFail "Du har inte fyllt i personnnummer, vänligen försök igen."
    | BS.null nEleg     = MergeKeep
    | otherwise =
        let nsc = normalizeNumber nContract
            nse = normalizeNumber nEleg
            dif = levenshtein (BS.toString nsc) (BS.toString nse)
        in if dif <= 3
            then MergeMatch
            else MergeFail $ "Personnnummer matchar inte: \"" ++ BS.toString nContract ++ "\" och \"" ++ BS.toString nEleg ++ "\"."

compareLastNames :: BS.ByteString -> BS.ByteString -> MergeResult
compareLastNames lnContract lnEleg
    | BS.null lnContract = MergeFail "Du har inte fyllt i efternamn, vänligen försök igen."
    | BS.null lnEleg = MergeKeep
    | levenshtein (map toLower (BS.toString lnContract)) (map toLower (BS.toString lnEleg)) <= 1 = MergeMatch
    | otherwise = MergeFail $ "Efternamn matchar inte: \"" ++ BS.toString lnContract ++ "\" och \"" ++ BS.toString lnEleg ++"\"."

--GHC.Unicode.toLower
-- import GHC.Unicode ( toLower )
--import qualified Data.ByteString.Lazy.Char8 as B


compareSigLinkToElegData :: SignatoryLink -> [(BS.ByteString, BS.ByteString)] -> Either (String, BS.ByteString, BS.ByteString, BS.ByteString) (Bool, Bool, Bool)
compareSigLinkToElegData sl attrs =
  -- compare information from document (and fields) to that obtained from BankID
  let contractFirst  = getFirstName sl
      contractLast   = getLastName sl
      contractNumber = getPersonalNumber sl
                
      elegFirst  = fieldvaluebyid (BS.fromString "Subject.GivenName")    attrs
      elegLast   = fieldvaluebyid (BS.fromString "Subject.Surname")      attrs
      elegNumber = fieldvaluebyid (BS.fromString "Subject.SerialNumber") attrs

  in mergeInfo (contractFirst, contractLast, contractNumber)
               (elegFirst,     elegLast,     elegNumber)
