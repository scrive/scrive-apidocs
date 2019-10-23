module Doc.TestInvariants (
    invariantProblems
  , documentInvariants
  ) where

import qualified Data.Text as T

import Doc.DocInfo
import Doc.DocStateData
import Doc.SignatoryLinkID
import InputValidation
import MinutesTime
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Monoid
import Utils.Prelude

invariantProblems :: UTCTime -> Document -> Maybe String
invariantProblems now document =
  case catMaybes $ map (\f -> f now document) documentInvariants of
    [] -> Nothing
    a  -> Just $ (show $ documentid document) ++ " : " ++ intercalate ";" a

{- |
   The invariants we want to test. Each returns Nothing if there is no problem,
    and Just message to describe a problem.

   UTCTime is the current time, in case an invariant depends on age.
 -}
documentInvariants :: [UTCTime -> Document -> Maybe String]
documentInvariants =
  [ documentHasOneAuthor
  , authorCannotBeAnApprover
  , noDeletedSigLinksForSigning
  , noSigningOrSeeingInPrep
  , connectedSigLinkOnTemplateOrPreparation
  , authorHasUser
  , signatoryLimit
                     -- Removing requirement that it is seen before signed. --Eric
                     -- , seenWhenSigned
  , allSignedWhenClosed
  , maxLengthOnFields
  , maxNumberOfPlacements
  , atLeastOneSignatory
  , notSignatoryNotSigned
  , maxCustomFields
  , closedWhenAllSigned
  , hasSignedAttachments
--                   , hasFirstName
--                   , hasLastName
  , hasValidEmail
  , hasAtMostOneOfEachTypeOfField
  , signatoryLinksHaveDifferentIDs
  ]

{- |
   Test the invariant that a document must have exactly one author.
-}
documentHasOneAuthor :: UTCTime -> Document -> Maybe String
documentHasOneAuthor _ document =
  let authors = (filter isAuthor $ documentsignatorylinks document)
  in
    assertInvariant
      ("document must have one author (has " ++ show (length authors) ++ ")")
    $  length authors
    == 1

{- |
   Test the invariant that an author cannot be an approver.
-}
authorCannotBeAnApprover :: UTCTime -> Document -> Maybe String
authorCannotBeAnApprover _ document =
  let authors = (filter isAuthor $ documentsignatorylinks document)
  in  assertInvariant "author cannot be an approver" $ all (not . isApprover) authors

{- |
   We don't expect to find any deleted documents in Pending state.
   Basically, you can't delete what needs to be signed.
 -}
noDeletedSigLinksForSigning :: UTCTime -> Document -> Maybe String
noDeletedSigLinksForSigning _ document =
  assertInvariant "document has a deleted siglink when it is due to be signed"
    $   (isPending document)
    --> none (isJust . signatorylinkdeleted) (documentsignatorylinks document)

{- |
  Preparation implies that no one has seen or signed the document
 -}
noSigningOrSeeingInPrep :: UTCTime -> Document -> Maybe String
noSigningOrSeeingInPrep _ document =
  assertInvariant "document has seen and/or signed siglinks when still in Preparation"
    $   isPreparation document
    --> none (isSignatoryAndHasSigned || hasSeen) (documentsignatorylinks document)

{- |
  Each of signatory links per document should have different id.
  Tests used to produce such documents.
-}
signatoryLinksHaveDifferentIDs :: UTCTime -> Document -> Maybe String
signatoryLinksHaveDifferentIDs _ document =
  let slids = map signatorylinkid (documentsignatorylinks document)
  in  assertInvariant ("some of document signatory links have same id: " ++ show slids)
        $ (all ((== 1) . length) . group . sort . filter ((/=) (unsafeSignatoryLinkID 0)))
            slids

{- |
   Template or Preparation implies only Author has user or company connected
 -}
connectedSigLinkOnTemplateOrPreparation :: UTCTime -> Document -> Maybe String
connectedSigLinkOnTemplateOrPreparation _ document =
  assertInvariant
      "document has siglinks (besides author) with User or Company when in Preparation or it's a Template"
    $   (isTemplate document || isPreparation document)
    --> (none (isJust . maybesignatory)
              (filter (not . isAuthor) (documentsignatorylinks document))
        )

{- |
   Author must have a user
 -}
authorHasUser :: UTCTime -> Document -> Maybe String
authorHasUser _ document =
  assertInvariant "author does not have a user connected."
    $   isJust
    $   join
    $   maybesignatory
    <$> (getAuthorSigLink document)

{- |
  Assert an upper bound on number of signatories.
 -}
signatoryLimit :: UTCTime -> Document -> Maybe String
signatoryLimit _ document =
  let maxsigs = 150
  in  assertInvariant
          (  show (length (documentsignatorylinks document))
          ++ " signatorylinks--max is "
          ++ show maxsigs
          )
        $  length (documentsignatorylinks document)
        <= maxsigs

{- | Closed implies all signatories have signed
 -}
allSignedWhenClosed :: UTCTime -> Document -> Maybe String
allSignedWhenClosed _ document =
  assertInvariant "some signatories are not signed when it is closed"
    $   isClosed document
    --> all (isSignatory --> isSignatoryAndHasSigned) (documentsignatorylinks document)

{- | All signed implies all closed
 -}
closedWhenAllSigned :: UTCTime -> Document -> Maybe String
closedWhenAllSigned _ document =
  assertInvariant "all signatories signed but doc is not closed"
    $ let sigLinks = documentsignatorylinks document
      in  (  any isSignatory sigLinks
          && all (isSignatory --> isSignatoryAndHasSigned) sigLinks
          )
            --> (isClosed document || isPreparation document || isDocumentError document)


{- | If a sig has signed, all his attachments are uploaded
 -}
hasSignedAttachments :: UTCTime -> Document -> Maybe String
hasSignedAttachments _ document =
  assertInvariant "a signatory has signed without attaching his requested attachment"
    $ all
        (   isSignatoryAndHasSigned
        --> (all (isJust . signatoryattachmentfile) . signatoryattachments)
        )
        (documentsignatorylinks document)

{- |
   Has signed implies has seen.

seenWhenSigned :: UTCTime -> Document -> Maybe String
seenWhenSigned _ document =
  assertInvariant "some signatories have signed but not seen" $
    all (isSignatoryAndHasSigned --> hasSeen) (documentsignatorylinks document)
-}

{- |
   max length of fields
 -}
maxLengthOnFields :: UTCTime -> Document -> Maybe String
maxLengthOnFields _ document =
  let maxlength = 512 :: Int
      lengths :: [Int]
      -- signature field can be longer than max
      lengths =
          [ T.length (fromMaybe "" $ fieldTextValue f)
          | s <- documentsignatorylinks document
          , f <- signatoryfields $ s
          , isJust $ fieldTextValue f
          , case fieldType f of
            SignatureFT -> False -- filter our signatures, they might be long
            _           -> True
          ]
      m = maximum (0 : lengths)
  in  assertInvariant
          ("some fields were too long: " ++ show m ++ ". max is " ++ show maxlength)
        $  m
        <= maxlength

{- |
   max number of placements per field
 -}
maxNumberOfPlacements :: UTCTime -> Document -> Maybe String
maxNumberOfPlacements _ document =
  let maxlength = 25 :: Int
      lengths :: [Int]
      lengths = concatMap (map (length . fieldPlacements) . signatoryfields)
                          (documentsignatorylinks document)
      m = maximum (0 : lengths)
  in  assertInvariant
          (  "document had too many placements: "
          ++ show m
          ++ ". max is "
          ++ show maxlength
          ++ " (25 * number of fields)"
          )
        $  m
        <= maxlength

{- |
   At least one signatory in Pending or Closed
 -}
atLeastOneSignatory :: UTCTime -> Document -> Maybe String
atLeastOneSignatory _ document =
  assertInvariant "there are no signatories, though doc is pending or closed"
    $   (isPending document || isClosed document)
    --> (any isSignatory (documentsignatorylinks document))


{- |
   If you're not a signatory, you shouldn't be signed
 -}
notSignatoryNotSigned :: UTCTime -> Document -> Maybe String
notSignatoryNotSigned _ document =
  assertInvariant "there are non-signatories/approvers who have signed/approved"
    $ (all ((not . (isSignatory || isApprover)) --> (not . isJust . maybesigninfo))
           (documentsignatorylinks document)
      )

{- |
   Maximum number of custom fields
 -}
maxCustomFields :: UTCTime -> Document -> Maybe String
maxCustomFields _ document =
  let maxfields = 250 :: Int
      fields    = map (length . filter (\f -> fieldType f == TextFT) . signatoryfields)
                      (documentsignatorylinks document)
      m = maximum (0 : fields)
  in  assertInvariant
          (  "there are signatories with too many custom fields: "
          ++ show m
          ++ ". maximum is "
          ++ show maxfields
          )
        $  m
        <= maxfields

-- the following should work in Pending and Closed

-- | First Name not null
_hasFirstName :: UTCTime -> Document -> Maybe String
_hasFirstName _ document = assertInvariant "has a signatory with no first name" $ all
  (\sl -> (isPending document || isClosed document) --> (not $ T.null $ getFirstName sl))
  (documentsignatorylinks document)

-- | Last Name not null
_hasLastName :: UTCTime -> Document -> Maybe String
_hasLastName _ document = assertInvariant "has a signatory with no last name" $ all
  (\sl -> (isPending document || isClosed document) --> (not $ T.null $ getLastName sl))
  (documentsignatorylinks document)

-- | Email looks like email
hasValidEmail :: UTCTime -> Document -> Maybe String
hasValidEmail _ document =
  assertInvariant
      (T.unpack $ "has a signatory with invalid email: " <> T.intercalate
        ", "
        (map getEmail (documentsignatorylinks document))
      )
    $ all
        (\sl ->
          (isPending document || isClosed document)
            --> (isGood $ asValidEmail $ getEmail sl)
        )
        (documentsignatorylinks document)

-- | Has only one of each type of field
hasAtMostOneOfEachTypeOfField :: UTCTime -> Document -> Maybe String
hasAtMostOneOfEachTypeOfField _ document =
  emptyToNothing
    $ intercalate ";"
    $ catMaybes
    $ for
        [ (NameFI (NameOrder 1))
        , (NameFI (NameOrder 2))
        , CompanyFI
        , PersonalNumberFI
        , CompanyNumberFI
        , EmailFI
        ]
    $ \t -> assertInvariant ("signatory with more than one " ++ show t) $ all
        (\sl -> 1 >= length (filter (\f -> fieldIdentity f == t) (signatoryfields sl)))
        (documentsignatorylinks document)

-- some helpers

assertInvariant :: String -> Bool -> Maybe String
assertInvariant _ True  = Nothing
assertInvariant s False = Just s
