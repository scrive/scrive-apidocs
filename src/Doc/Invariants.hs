module Doc.Invariants where

import Doc.DocStateData
import Util.SignatoryLinkUtils

import Data.List
import Data.Maybe

listInvariantProblems :: [Document] -> [String]
listInvariantProblems docs = catMaybes $ map invariantProblems docs

invariantProblems :: Document -> Maybe String
invariantProblems document =
  case catMaybes $ map (flip ($) document) documentInvariants of
    [] -> Nothing
    a  -> Just $ (show $ documentid document) ++ ": " ++ intercalate ";" a

documentInvariants :: [Document -> Maybe String]
documentInvariants = [
  documentHasOneAuthor
                     ]

{- |
   Test the invariant that a document must have exactly one author.
-}
documentHasOneAuthor :: Document -> Maybe String
documentHasOneAuthor document =
  case filter isAuthor $ documentsignatorylinks document of
    [_] -> Nothing
    a -> Just $ "document must have one author (has " ++ show (length a) ++ ")"
