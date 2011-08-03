module Doc.Invariants (listInvariantProblems, invariantProblems, documentInvariants) where

import Doc.DocStateData
import Util.SignatoryLinkUtils
import MinutesTime
import Doc.DocInfo
import Doc.DocUtils
import Misc

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
                     , noDeletedSigLinksForSigning
                     , noSigningOrSeeingInPrep
                     , connectedSigLinkOnTemplateOrPreparation
                     , authorHasUser
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

{- |
   We don't expect to find any deleted documents in Pending or AwaitingAuthor
   Basically, you can't delete what needs to be signed.
 -}
noDeletedSigLinksForSigning :: MinutesTime -> Document -> Maybe String
noDeletedSigLinksForSigning _ document
  | documentstatus document `elem` [Pending, AwaitingAuthor] &&
    any isDeletedFor (documentsignatorylinks document)          = Just "document has a deleted siglink when it is due to be signed"
  | otherwise = Nothing

noSigningOrSeeingInPrep :: MinutesTime -> Document -> Maybe String
noSigningOrSeeingInPrep _ document
  | not $ isPreparation document = Nothing
  | any (hasSigned ||^ hasSeen) (documentsignatorylinks document) 
      = Just "document has seen and/or signed siglinks when still in Preparation"
  | otherwise = Nothing

connectedSigLinkOnTemplateOrPreparation :: MinutesTime -> Document -> Maybe String
connectedSigLinkOnTemplateOrPreparation _ document
  | not (isTemplate document || isPreparation document) = Nothing
  | any (hasUser ||^ hasCompany) (filter (not . isAuthor) (documentsignatorylinks document))
      = Just "document has siglinks (besides author) with User or Company when in Preparation or it's a Template"
  | otherwise = Nothing

authorHasUser :: MinutesTime -> Document -> Maybe String
authorHasUser _ document = case getAuthorSigLink document of
  Just sl | not $ hasUser sl -> Just "author does not have a user connected."
  _ -> Nothing
