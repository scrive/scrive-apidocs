module ScriveByMail.Parse where

import Control.Logic
import Data.Maybe
import Data.String.Utils
import Control.Monad
import Control.Applicative
import Utils.Prelude
import Data.List
import Utils.String
import Data.Char
import Doc.DocStateData

-- Simple Mail API

-- | like lookup, but compares with maxLev 2 instead
--     of ==
levLookup :: String -> [(String, a)] -> Maybe a
levLookup _ []                                    = Nothing
levLookup k1 ((k2, v):_) | maxLev k1 k2 2         = Just v
levLookup k1 (_:ps)                               = levLookup k1 ps
  
data SimpleMailError = NoColon String
                     | UnknownKey String
                     | MissingField String
                     | NoSignatory
                     | CombinedError [String] [SimpleMailError]
                       
instance Show SimpleMailError where
  show (NoColon l) = "Each line must be of the form key:value; this line had no colon " ++ l
  show (UnknownKey k) = "This key was not recognized: " ++ k
  show (MissingField k) = "This required key was missing: " ++ k
  show NoSignatory = "The body of the email was empty; we cannot send a contract with no signatories"
  show (CombinedError s es) = 
    let len = foldl max 20 (map length s)
        rule = take len $ repeat '-'
    in "For this signatory:<br /><br />\n" ++
       rule ++ "<br />\n" ++
       intercalate "<br />\n" s ++
       "<br />\n" ++ 
       rule ++ "<br /><br />\n" ++
       "There were the following errors: <br />\n  * " ++
       intercalate "<br />\n  * " (map show es)

noColonCheck :: String -> Maybe SimpleMailError
noColonCheck l | isNothing $ find (== ':') l = Just $ NoColon l
noColonCheck _ = Nothing

allStrings :: [[String]]
allStrings = [firstNameStrings 
             ,lastNameStrings
             ,emailStrings
             ,companyStrings
             ,orgStrings
             ,persStrings]

firstNameStrings :: [String] 
firstNameStrings = ["first name"]

lastNameStrings :: [String]
lastNameStrings = ["last name"]

emailStrings :: [String]
emailStrings = ["email"]

companyStrings :: [String]
companyStrings = ["company", "organization"]

orgStrings :: [String]
orgStrings = [a ++ s ++ b | a <- ["org", "cmp"]
                          , s <- [" ", ""]
                          , b <- ["number", "num", "nr"]]
             ++ [a ++ " number" | a <- ["organization", "company"]]
                                    
             
persStrings :: [String]
persStrings = [a ++ s ++ b | a <- ["personal", "pers"]
                           , s <- [" ", ""]
                           , b <- ["number", "num", "nr"]]

unknownKeyCheck :: String -> Maybe SimpleMailError
unknownKeyCheck l | isJust $ find (== ':') l = 
  let (k, _) = break (== ':') l
  in if none (\a -> maxLev (map toLower k) a 2) (concat allStrings)
     then Just $ UnknownKey k
     else Nothing
unknownKeyCheck _ = Nothing

-- | Takes one line of a simple email and returns the errors                                                    
-- in that line
getParseErrorsLine :: String -> [SimpleMailError]
getParseErrorsLine l = catMaybes $ map ($ l) [unknownKeyCheck
                                             ,noColonCheck
                                             ]
                       
getParseErrorsSig :: [String] -> Maybe SimpleMailError
getParseErrorsSig ls = let unks = concatMap getParseErrorsLine ls
                           pairs = [(strip $ toLower <$> k, strip $ (drop 1) v)| (k, v) <- map (break (== ':')) ls]
                           fstname = levLookup "first name"          pairs
                           sndname = levLookup "last name"           pairs
                           email   = levLookup "email"               pairs
                           missings = [MissingField s | (s, Nothing) <- [("First name", fstname), 
                                                                         ("Last name",  sndname),
                                                                         ("Email",      email)]]
                       in case unks ++ missings of
                         [] -> Nothing
                         es -> Just $ CombinedError ls es
                         
joinColons :: [String] -> [String]
joinColons []                        = []
joinColons (s:t:ss) | endswith ":" s = joinColons $ (s ++ t) : ss
joinColons (s:ss)                    = s : joinColons ss

splitSignatories :: String -> [[String]]
splitSignatories mailbody =
  let sigstrings' = joinColons $ map strip $ lines $ strip mailbody
      sigstrings  = takeWhile ((== "") ||^ (elem ':')) sigstrings'           
      lss  = filter (/= []) $ split [""] sigstrings
      lss' = map (filter (/= "")) $ filter (/= []) $ split ["", ""] sigstrings
  in if any (\s->1==length s) lss
     then lss'
     else lss

getParseErrorsEmail :: String -> [SimpleMailError]
getParseErrorsEmail mailbody = 
  if strip mailbody == ""
  then [NoSignatory]
  else catMaybes $ map getParseErrorsSig $ splitSignatories mailbody

parseSignatory :: [String] -> Maybe SignatoryDetails
parseSignatory sig = 
  let ls = sig
      pairs = [(strip $ toLower <$> k, strip $ (drop 1) v)| (k, v) <- map (break (== ':')) ls]
      fstname = msum $ map (flip levLookup $ pairs) firstNameStrings
      sndname = msum $ map (flip levLookup $ pairs) lastNameStrings
      email   = msum $ map (flip levLookup $ pairs) emailStrings
      company = msum $ map (flip levLookup $ pairs) companyStrings
      cmpnr   = msum $ map (flip levLookup $ pairs) orgStrings
      prsnr   = msum $ map (flip levLookup $ pairs) persStrings
  in
   if all isJust [fstname, sndname, email]
   then let ss = [SignatoryField FirstNameFT (fromJust fstname) [],
                  SignatoryField LastNameFT  (fromJust sndname) [],
                  SignatoryField EmailFT     (fromJust email  ) []] ++
                 [SignatoryField CompanyFT   (a               ) [] | Just a <- [company], not $ null a] ++
                 [SignatoryField CompanyNumberFT   (a               ) [] | Just a <- [cmpnr], not $ null a] ++
                 [SignatoryField PersonalNumberFT   (a               ) [] | Just a <- [prsnr], not $ null a]
        in if length ss == length pairs 
           then Just $ SignatoryDetails (SignOrder 0) ss False False
           else Nothing
   else Nothing

parseSimpleEmail :: String -> String -> Either String (String, [SignatoryDetails])
parseSimpleEmail subject mailbody =  
  case getParseErrorsEmail mailbody of
    [] -> if strip subject == ""
          then Left "The subject of the email becomes the title. The subject you sent was blank. Please add a subject."
          else let sigs = map parseSignatory $ splitSignatories mailbody
               in Right $ (strip subject,
                           catMaybes sigs)
    es -> Left $ intercalate "<br /><br />\n" (map show es)
