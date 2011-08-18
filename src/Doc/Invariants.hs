module Doc.Invariants (listInvariantProblems, invariantProblems, documentInvariants) where

import Doc.DocStateData
import Util.SignatoryLinkUtils
import MinutesTime
import Doc.DocInfo
import Doc.DocUtils
import Misc
import Util.HasSomeUserInfo
import InputValidation

import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)


listInvariantProblems :: MinutesTime -> [Document] -> [String]
listInvariantProblems now docs = catMaybes $ map (invariantProblems now) docs

invariantProblems :: MinutesTime -> Document -> Maybe String
invariantProblems now document =
  case catMaybes $ map (\f -> f now document) documentInvariants of
    [] -> Nothing
    a  -> Just $ (show $ documentid document) ++ " : " ++ intercalate ";" a

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
                     , maxLengthOnFields
                     , maxNumberOfPlacements
                     , awaitingAuthorAuthorNotSigned
                     , atLeastOneSignatory
                     , notSignatoryNotSigned
                     , maxCustomFields
                     , closedWhenAllSigned
                     , hasSignedAttachments
                     , hasFirstName
                     , hasLastName
                     , hasValidEmail
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
  isClosed document =>> all (isSignatory =>>^ hasSigned) (documentsignatorylinks document)
  
{- | All signed implies all closed
 -}
closedWhenAllSigned :: MinutesTime -> Document -> Maybe String
closedWhenAllSigned _ document =
  assertInvariant "all signatories signed but doc is not closed" $
  (any isSignatory (documentsignatorylinks document) && 
   all (isSignatory =>>^ hasSigned) (documentsignatorylinks document)) =>> 
  (isClosed document || isPreparation document)

  
{- | If a sig has signed, all his attachments are uploaded
 -}
hasSignedAttachments :: MinutesTime -> Document -> Maybe String
hasSignedAttachments _ document =
  assertInvariant "a signatory has signed without attaching his requested attachment" $
  all (hasSigned =>>^ (all (isJust . signatoryattachmentfile) . (sigAttachmentsForEmail document . getEmail))) (documentsignatorylinks document)
       
sigAttachmentsForEmail :: Document -> BS.ByteString -> [SignatoryAttachment]
sigAttachmentsForEmail doc email = filter ((== email) . signatoryattachmentemail) (documentsignatoryattachments doc)
       
{- |
   Has signed implies has seen.
 -}
seenWhenSigned :: MinutesTime -> Document -> Maybe String
seenWhenSigned _ document =
  assertInvariant "some signatories have signed but not seen" $
    all (hasSigned =>>^ hasSeen) (documentsignatorylinks document)
    
{- |
   max length of fields
 -}
maxLengthOnFields :: MinutesTime -> Document -> Maybe String
maxLengthOnFields _ document =
  let maxlength = 512 
      assertMaxLength s = BS.length s <= maxlength in
  assertInvariant ("some fields were too long. max is " ++ show maxlength) $
    all (\sl -> let sd = signatorydetails sl in
          assertMaxLength (signatoryfstname sd) &&
          assertMaxLength (signatorysndname sd) &&
          assertMaxLength (signatorycompany sd) &&
          assertMaxLength (signatorypersonalnumber sd) &&
          assertMaxLength (signatorycompanynumber sd) &&
          assertMaxLength (signatoryemail sd) &&
          all (\fd -> assertMaxLength (fieldlabel fd) &&
                      assertMaxLength (fieldvalue fd))
              (signatoryotherfields sd))
        (documentsignatorylinks document)
    
{- |
   max number of placements per field
 -}
maxNumberOfPlacements :: MinutesTime -> Document -> Maybe String
maxNumberOfPlacements _ document =
  let maxlength = 25 * totalfields
      totalfields = sum (map countplacements (documentsignatorylinks document))
      countplacements sl =
        let sd = signatorydetails sl in
        length (signatoryfstnameplacements sd) +
        length (signatorysndnameplacements sd) +
        length (signatorycompanyplacements sd) +
        length (signatoryemailplacements sd) +
        length (signatorypersonalnumberplacements sd) +
        length (signatorycompanynumberplacements sd) +
        sum (map (length . fieldplacements) (signatoryotherfields sd))
  in
   assertInvariant ("document had too many placements. max is " ++ show maxlength ++ " (25 * number of fields)") $
   totalfields <= maxlength
    
{- |
   AwaitingAuthor implies Author has not signed
 -}
awaitingAuthorAuthorNotSigned :: MinutesTime -> Document -> Maybe String
awaitingAuthorAuthorNotSigned _ document =
  assertInvariant "Author has signed but still in AwaitingAuthor" $
    (isAwaitingAuthor document =>> (not $ hasSigned $ getAuthorSigLink document))
    
{- |
   At least one signatory in Pending AwaitingAuthor or Closed
 -}
atLeastOneSignatory :: MinutesTime -> Document -> Maybe String
atLeastOneSignatory _ document =
  assertInvariant "there are no signatories, though doc is pending, awaiting author, or closed" $
    (isPending document || isAwaitingAuthor document || isClosed document) =>>
    (any isSignatory (documentsignatorylinks document))
    
{- |
   If you're not a signatory, you shouldn't be signed
 -}
notSignatoryNotSigned :: MinutesTime -> Document -> Maybe String
notSignatoryNotSigned _ document =
  assertInvariant "there are non-signatories who have signed" $
    (all ((not . isSignatory) =>>^ (not . hasSigned)) (documentsignatorylinks document))
    
{- |
   Maximum number of custom fields
 -}
maxCustomFields :: MinutesTime -> Document -> Maybe String
maxCustomFields _ document =
  let maxfields = 250 
      assertMaximum sl = length (signatoryotherfields $ signatorydetails sl) <= maxfields in
  assertInvariant ("there are signatories with too many custom fields. maximum is " ++ show maxfields) $
    all assertMaximum (documentsignatorylinks document)

-- the following should work in Pending, Closed, AwaitingAuthor

-- | First Name not null
hasFirstName :: MinutesTime -> Document -> Maybe String
hasFirstName _ document =
  assertInvariant "has a signatory with no first name" $
    all (\sl -> (isPending document || isClosed document || isAwaitingAuthor document) =>>
                (not $ null $ BS.toString$ getFirstName sl))
        (documentsignatorylinks document)

-- | Last Name not null
hasLastName :: MinutesTime -> Document -> Maybe String
hasLastName _ document =
  assertInvariant "has a signatory with no last name" $
    all (\sl -> (isPending document || isClosed document || isAwaitingAuthor document) =>>
                (not $ null $ BS.toString $ getLastName sl))
        (documentsignatorylinks document)

-- | Email looks like email
hasValidEmail :: MinutesTime -> Document -> Maybe String
hasValidEmail _ document =
  assertInvariant "has a signatory with invalid email" $
    all (\sl -> (isPending document || isClosed document || isAwaitingAuthor document) =>>
                (isGood $ asValidEmail $ BS.toString $ getEmail sl))
        (documentsignatorylinks document)

-- some helpers  
       
assertInvariant :: String -> Bool -> Maybe String
assertInvariant _ True = Nothing
assertInvariant s False  = Just s


-- | Given the time now, is doc older than minutes.
olderThan :: MinutesTime -> Document -> Int -> Bool
olderThan now doc minutes = toMinutes now - toMinutes (documentctime doc) >= minutes
