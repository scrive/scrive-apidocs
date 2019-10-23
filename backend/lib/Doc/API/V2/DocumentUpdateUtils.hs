module Doc.API.V2.DocumentUpdateUtils (
    applyDraftDataToDocument
  , clearDocFields
  , adjustFieldAndPlacementsAfterRemovingPages
) where

import Control.Conditional (unlessM, whenM)
import Control.Monad.Base
import Log
import qualified Control.Exception.Lifted as E

import API.V2
  ( apiError, documentStateError, requestParameterInvalid, serverError )

import DB
import DB.TimeZoneName
import Doc.DocInfo (isPreparation)
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument)
import Doc.DocUtils
import Doc.Model
import Doc.SignatoryFieldID
import File.Storage (getFileContents)
import Kontra
import Log.Identifier
import Util.Actor
import Util.HasSomeUserInfo
import Util.PDFUtil

checkDraftTimeZoneName ::  (Kontrakcja m) =>  Document -> m ()
checkDraftTimeZoneName draft = do
  void $  (mkTimeZoneName $ toString (documenttimezonename draft))
    `E.catch` (\(_::E.SomeException) ->  apiError $ requestParameterInvalid "document" "timezone name is invalid")

applyDraftDataToDocument :: (Kontrakcja m, DocumentMonad m) =>  Document -> Actor -> m ()
applyDraftDataToDocument draft actor = do
    checkDraftTimeZoneName draft
    unlessM (isPreparation <$> theDocument) $ do
      theDocument >>= \doc -> logInfo "API V2 - Document is not in preparation" $ logObject_ doc
      apiError $ serverError "Could not apply draft data to document as document is not in preparation."
    void $ theDocument >>= \doc -> dbUpdate $ UpdateDraft doc{
                                  documenttitle = documenttitle draft
                                , documentinvitetext = documentinvitetext draft
                                , documentconfirmtext = documentconfirmtext draft
                                , documentdaystosign = documentdaystosign draft
                                , documentdaystoremind = min (documentdaystosign draft) <$> documentdaystoremind draft
                                , documentshowheader = documentshowheader draft
                                , documentshowpdfdownload = documentshowpdfdownload draft
                                , documentshowrejectoption = documentshowrejectoption draft
                                , documentallowrejectreason = documentallowrejectreason draft
                                , documentshowfooter = documentshowfooter draft
                                , documentisreceipt = documentisreceipt draft
                                , documentlang = documentlang draft
                                , documenttags = documenttags draft
                                , documentapiv2callbackurl = documentapiv2callbackurl draft
                                , documenttimezonename = documenttimezonename draft
                                , documentshowarrow = documentshowarrow draft
                                , documentfolderid = documentfolderid draft
                                } actor
    -- Only allow transition from 'unsaveddraft: true' to 'unsaveddraft: false'
    whenM ((\doc -> (documentunsaveddraft doc) && not (documentunsaveddraft draft)) <$> theDocument) $ do
         dbUpdate $ SetDocumentUnsavedDraft False
    whenM ((\doc -> isTemplate draft && (not $ isTemplate doc)) <$> theDocument) $ do
         dbUpdate $ TemplateFromDocument actor
    documentsignatorylinks <$> theDocument >>= \siglinks -> case (mergeAuthorDetails siglinks $ mergeSignatoriesIDs siglinks $ documentsignatorylinks draft) of
         Nothing   -> apiError $ requestParameterInvalid "document" "parties list is empty"
         Just sigs -> do
           res <- dbUpdate $ ResetSignatoryDetails sigs actor
           unless res $ apiError $ serverError "applyDraftDataToDocument failed"

mergeAuthorDetails :: [SignatoryLink] ->[SignatoryLink] -> Maybe [SignatoryLink]
mergeAuthorDetails sigs nsigs =
          let
            setConstantDetails a =  (\s -> s {signatoryfields = replaceName1 (getFirstName a) $ signatoryfields s}) .
                                    (\s -> s {signatoryfields = replaceName2 (getLastName a) $ signatoryfields s}) .
                                    (\s -> s {signatoryfields = replaceEmail (getEmail a) $ signatoryfields s}) .
                                    (\s -> s {maybesignatory = maybesignatory a}) .  -- We need to be sure that we will not disconnect author
                                    (\s -> s {signatoryisauthor = True})             -- And we make sure that he is actually author

            replaceName1 a [] = [SignatoryNameField $ NameField {
              snfID = unsafeSignatoryFieldID 0,
              snfValue = a,
              snfNameOrder = NameOrder 1,
              snfPlacements =[],
              snfObligatory = True,
              snfShouldBeFilledBySender = False
            }]
            replaceName1 a ((SignatoryNameField nf@(NameField {snfNameOrder = NameOrder 1})):fs) = ((SignatoryNameField $ nf {snfValue = a}):fs)
            replaceName1 a (f:fs) = f:(replaceName1 a fs)

            replaceName2 a [] = [SignatoryNameField $ NameField {
              snfID = unsafeSignatoryFieldID 0,
              snfValue = a,
              snfNameOrder = NameOrder 2,
              snfPlacements =[],
              snfObligatory = True,
              snfShouldBeFilledBySender = False
            }]
            replaceName2 a ((SignatoryNameField nf@(NameField {snfNameOrder = NameOrder 2})):fs) = ((SignatoryNameField $ nf {snfValue = a}):fs)
            replaceName2 a (f:fs) = f:(replaceName2 a fs)

            replaceEmail a [] = [SignatoryEmailField $ EmailField {
              sefID = unsafeSignatoryFieldID 0,
              sefValue = a,
              sefPlacements =[],
              sefObligatory = True,
              sefShouldBeFilledBySender = False,
              sefEditableBySignatory = False

            }]
            replaceEmail a ((SignatoryEmailField ef):fs) = ((SignatoryEmailField $ ef {sefValue = a}):fs)
            replaceEmail a (f:fs) = f:(replaceEmail a fs)

          in case (sigs, nsigs) of
               (asig:_, nasig : nsigs') -> Just $ (setConstantDetails asig nasig) : nsigs'
               _ -> Nothing

mergeSignatoriesIDs :: [SignatoryLink] ->[SignatoryLink] -> [SignatoryLink]
mergeSignatoriesIDs (s:ss) (ns:nss) = (ns {signatorylinkid = signatorylinkid s}) : (mergeSignatoriesIDs ss nss)
mergeSignatoriesIDs _ ns = ns

-- This function removes:
-- * placements that are on non-existing pages
-- * non-standard fields that have no placements
clearDocFields :: (Kontrakcja m, DocumentMonad m) => Actor -> m ()
clearDocFields actor = do
  mfile <- fileFromMainFile =<< documentfile <$> theDocument
  case mfile of
    Nothing -> apiError $ documentStateError "Document does not have a main file"
    Just file -> do
      content <- getFileContents file
      enop <- liftBase $ getNumberOfPDFPages content
      case enop of
        Left _ -> apiError $ serverError "Can't extract number of pages from PDF"
        Right numOfPages -> do
          sigs <- documentsignatorylinks <$> theDocument

          let clearSigFields sig = sig { signatoryfields = filter validField $ map clearField $ signatoryfields sig }

              validField (SignatoryNameField _) = True
              validField (SignatoryCompanyField _) = True
              validField (SignatoryPersonalNumberField _) = True
              validField (SignatoryCompanyNumberField _) = True
              validField (SignatoryEmailField _) = True
              validField (SignatoryMobileField _) = True
              validField (SignatoryTextField _) = True
              validField (SignatoryCheckboxField schf) = not $ null $ schfPlacements schf
              validField (SignatorySignatureField ssf) = not $ null $ ssfPlacements ssf
              validField (SignatoryRadioGroupField srgf) = not $ null $ srgfPlacements srgf

              clearField (SignatoryNameField snf) = SignatoryNameField $ snf { snfPlacements = filter validPlacement $ snfPlacements snf }
              clearField (SignatoryCompanyField scf) = SignatoryCompanyField $ scf { scfPlacements = filter validPlacement $ scfPlacements scf }
              clearField (SignatoryPersonalNumberField spnf) = SignatoryPersonalNumberField $ spnf { spnfPlacements = filter validPlacement $ spnfPlacements spnf }
              clearField (SignatoryCompanyNumberField scnf) = SignatoryCompanyNumberField $ scnf { scnfPlacements = filter validPlacement $ scnfPlacements scnf }
              clearField (SignatoryEmailField sef) = SignatoryEmailField $ sef { sefPlacements = filter validPlacement $ sefPlacements sef }
              clearField (SignatoryMobileField smf) = SignatoryMobileField $ smf { smfPlacements = filter validPlacement $ smfPlacements smf }
              clearField (SignatoryTextField stf) = SignatoryTextField $ stf { stfPlacements = filter validPlacement $ stfPlacements stf }
              clearField (SignatoryCheckboxField schf) = SignatoryCheckboxField $ schf { schfPlacements = filter validPlacement $ schfPlacements schf }
              clearField (SignatorySignatureField ssf) = SignatorySignatureField $ ssf { ssfPlacements = filter validPlacement $ ssfPlacements ssf }
              clearField (SignatoryRadioGroupField srgf) = SignatoryRadioGroupField $ srgf { srgfPlacements = filter validPlacement $ srgfPlacements srgf }

              validPlacement fp = fromIntegral (placementpage fp) <= numOfPages

              sigs' = map clearSigFields sigs

          res <- dbUpdate $ ResetSignatoryDetails sigs' actor
          unless res $ apiError $ serverError "clearing document fields failed"


-- Utils for removing pages
adjustFieldAndPlacementsAfterRemovingPages :: (Kontrakcja m, DocumentMonad m) =>  [Int] -> Actor -> m ()
adjustFieldAndPlacementsAfterRemovingPages pages actor = do
  sigs <- documentsignatorylinks <$> theDocument
  let revSortedPages = reverse $ sort $ pages
  res <- dbUpdate $ ResetSignatoryDetails (map (adjustFieldAndPlacementsAfterRemovingPagesForSignatory revSortedPages) sigs) actor
  unless res $
    apiError $ serverError "adjustFieldAndPlacementsAfterRemovingPages failed"

adjustFieldAndPlacementsAfterRemovingPagesForSignatory :: [Int] -> SignatoryLink -> SignatoryLink
adjustFieldAndPlacementsAfterRemovingPagesForSignatory [] sl =  sl
adjustFieldAndPlacementsAfterRemovingPagesForSignatory (highestpageno:otherpages) sl = adjustFieldAndPlacementsAfterRemovingPagesForSignatory otherpages $
  sl { signatoryfields = mapMaybe (adjustFieldAndPlacementsAfterRemovingPage highestpageno) $ signatoryfields sl}

adjustFieldAndPlacementsAfterRemovingPage :: Int -> SignatoryField -> Maybe SignatoryField
adjustFieldAndPlacementsAfterRemovingPage page slf = case slf of
  SignatoryNameField  f -> Just $ SignatoryNameField $ f {snfPlacements = adjustPlacementsAfterRemovingPage page $ snfPlacements f}
  SignatoryEmailField f -> Just $ SignatoryEmailField $ f {sefPlacements = adjustPlacementsAfterRemovingPage page $ sefPlacements f}
  SignatoryMobileField f -> Just $ SignatoryMobileField $ f {smfPlacements = adjustPlacementsAfterRemovingPage page $ smfPlacements f}
  SignatoryCompanyField f -> Just $ SignatoryCompanyField $ f {scfPlacements = adjustPlacementsAfterRemovingPage page $ scfPlacements f}
  SignatoryPersonalNumberField f -> Just $ SignatoryPersonalNumberField $ f {spnfPlacements = adjustPlacementsAfterRemovingPage page $ spnfPlacements f}
  SignatoryCompanyNumberField f -> Just $ SignatoryCompanyNumberField $ f {scnfPlacements = adjustPlacementsAfterRemovingPage page $ scnfPlacements f}
  SignatoryTextField f -> Just $ SignatoryTextField $ f {stfPlacements = adjustPlacementsAfterRemovingPage page $ stfPlacements f}
  SignatoryCheckboxField f -> case (adjustPlacementsAfterRemovingPage page $ schfPlacements f) of
    [] -> Nothing
    notEmptyPlacementsList -> Just $ SignatoryCheckboxField $ f {schfPlacements = notEmptyPlacementsList }
  SignatorySignatureField f -> case (adjustPlacementsAfterRemovingPage page $ ssfPlacements f) of
    [] -> Nothing
    notEmptyPlacementsList -> Just $ SignatorySignatureField $ f {ssfPlacements = notEmptyPlacementsList }
  SignatoryRadioGroupField f -> case (adjustPlacementsAfterRemovingPage page $ srgfPlacements f) of
    [] -> Nothing
    notEmptyPlacementsList -> Just $ SignatoryRadioGroupField $ f {srgfPlacements = notEmptyPlacementsList }


adjustPlacementsAfterRemovingPage :: Int -> [FieldPlacement] -> [FieldPlacement]
adjustPlacementsAfterRemovingPage page ps = mapMaybe (adjustPlacementAfterRemovingPage page) ps

adjustPlacementAfterRemovingPage :: Int -> FieldPlacement -> Maybe FieldPlacement
adjustPlacementAfterRemovingPage page placement =
  if (placementpage placement < fromIntegral page)
    then Just $ placement
    else if (placementpage placement > fromIntegral page)
      then Just $ placement {placementpage = placementpage placement - 1}
      else Nothing
