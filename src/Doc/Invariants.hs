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
                     , signatoryLimit
                     , seenWhenSigned
                     , allSignedWhenClosed
                     ]

{- |
   Test the invariant that a document must have exactly one author.
-}
documentHasOneAuthor :: MinutesTime -> Document -> Maybe String
documentHasOneAuthor _ document =
  let authors = (filter isAuthor $ documentsignatorylinks document) in
  assertInvariant ("document must have one author (has " ++ show (length authors) ++ ")") $
    length authors == 1

{- |
   A document older than one hour implies it has at least one file.
   We must wait some time because we don't do NewDocument and AttachFile atomically. There could be some
   in between.
 -}
oldishDocumentHasFiles :: MinutesTime -> Document -> Maybe String
oldishDocumentHasFiles now document =
  assertInvariant "document must have files if older than one hour" $
    olderThan now document 60 =>> (length (documentfiles document) > 0)

{- |
   We don't expect to find any deleted documents in Pending or AwaitingAuthor
   Basically, you can't delete what needs to be signed.
 -}
noDeletedSigLinksForSigning :: MinutesTime -> Document -> Maybe String
noDeletedSigLinksForSigning _ document =
  assertInvariant "document has a deleted siglink when it is due to be signed" $
    (isPending document || isAwaitingAuthor document) =>>
    none isDeletedFor (documentsignatorylinks document)

{- |
  Preparation implies that no one has seen or signed the document
 -}
noSigningOrSeeingInPrep :: MinutesTime -> Document -> Maybe String
noSigningOrSeeingInPrep _ document =
  assertInvariant "document has seen and/or signed siglinks when still in Preparation" $
    isPreparation document =>> 
    none (hasSigned ||^ hasSeen) (documentsignatorylinks document)

{- |
   Template or Preparation implies only Author has user or company connected
 -}
connectedSigLinkOnTemplateOrPreparation :: MinutesTime -> Document -> Maybe String
connectedSigLinkOnTemplateOrPreparation _ document =
  assertInvariant "document has siglinks (besides author) with User or Company when in Preparation or it's a Template" $
    (isTemplate document || isPreparation document) =>>
    (none (hasUser ||^ hasCompany) (filter (not . isAuthor) (documentsignatorylinks document)))

{- |
   Author must have a user
 -}
authorHasUser :: MinutesTime -> Document -> Maybe String
authorHasUser _ document = 
  assertInvariant "author does not have a user connected." $
    hasUser (getAuthorSigLink document)

{- |
  Assert an upper bound on number of signatories.
 -}
signatoryLimit :: MinutesTime -> Document -> Maybe String
signatoryLimit _ document = 
  let maxsigs = 150 in
  assertInvariant (show (length (documentsignatorylinks document)) ++ " signatorylinks--max is " ++ show maxsigs) $
    length (documentsignatorylinks document) <= maxsigs

{- | Closed implies all signatories have signed
 -}
allSignedWhenClosed :: MinutesTime -> Document -> Maybe String
allSignedWhenClosed _ document =
  assertInvariant "some signatories are not signed when it is closed" $ 
  isClosed document =>> all hasSigned (filter isSignatory (documentsignatorylinks document))
       
{- |
   Has signed implies has seen.
 -}
seenWhenSigned :: MinutesTime -> Document -> Maybe String
seenWhenSigned _ document =
  assertInvariant "some signatories have signed but not seen" $
    all (hasSigned =>>^ hasSeen) (documentsignatorylinks document)

-- some helpers  
       
assertInvariant :: String -> Bool -> Maybe String
assertInvariant _ False = Nothing
assertInvariant s True  = Just s

none :: (a -> Bool) -> [a] -> Bool
none f l = not $ any f l

-- | Simple logical inference operator (arrow)
(=>>) :: Bool -> Bool -> Bool
(=>>) a b = not a || b

-- | Higher order inference
(=>>^) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(=>>^) a b = \x -> a x =>> b x

-- | Given the time now, is doc older than minutes.
olderThan :: MinutesTime -> Document -> Int -> Bool
olderThan now doc minutes = toMinutes now - toMinutes (documentctime doc) >= minutes
