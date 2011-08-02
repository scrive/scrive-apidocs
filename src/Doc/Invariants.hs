module Doc.Invariants where

import Doc.DocStateData
import Util.SignatoryLinkUtils
import MinutesTime

import Data.List
import Data.Maybe

listInvariantProblems :: MinutesTime -> [Document] -> [String]
listInvariantProblems now docs = catMaybes $ map (invariantProblems now) docs

invariantProblems :: MinutesTime -> Document -> Maybe String
invariantProblems now document =
  case catMaybes $ map (\f -> f now document) documentInvariants of
    [] -> Nothing
    a  -> Just $ (show $ documentid document) ++ ": " ++ intercalate ";" a

{- |
   The invariants we want to test. Each returns Nothing if there is no problem,
   and Just message to describe a problem.

   MinutesTime is the current time, in case an invariant depends on age.
 -} 
documentInvariants :: [MinutesTime -> Document -> Maybe String]
documentInvariants = [ documentHasOneAuthor
                     , oldishDocumentHasFiles
                     ]

{- |
   Test the invariant that a document must have exactly one author.
-}
documentHasOneAuthor :: MinutesTime -> Document -> Maybe String
documentHasOneAuthor _ document =
  case filter isAuthor $ documentsignatorylinks document of
    [_] -> Nothing
    a -> Just $ "document must have one author (has " ++ show (length a) ++ ")"

{- |
   Any document older than one hour that does not have any files is a problem.
   We must wait some time because we don't do NewDocument and AttachFile atomically. There could be some
   in between.
 -}
oldishDocumentHasFiles :: MinutesTime -> Document -> Maybe String
oldishDocumentHasFiles now document 
  | toMinutes now - toMinutes (documentctime document) < 60 = Nothing
  | documentfiles document == [] = Just "document must have files if older than one hour"
  | otherwise = Nothing


