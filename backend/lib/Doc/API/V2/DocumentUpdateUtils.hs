module Doc.API.V2.DocumentUpdateUtils (
 applyDraftDataToDocument
) where

import Control.Conditional (unlessM, whenM)
import Data.Functor
import Log
import qualified Control.Exception.Lifted as E

import API.V2 (apiError, requestParameterInvalid, serverError)
import DB
import DB.TimeZoneName
import Doc.DocInfo (isPreparation)
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument)
import Doc.DocUtils
import Doc.Model
import Doc.SignatoryFieldID
import Kontra
import KontraPrelude
import Util.Actor
import Util.HasSomeUserInfo

checkDraftTimeZoneName ::  (Kontrakcja m) =>  Document -> m ()
checkDraftTimeZoneName draft = do
  void $  (mkTimeZoneName $ toString (documenttimezonename draft))
    `E.catch` (\(_::E.SomeException) ->  apiError $ requestParameterInvalid "document" "timezone name is invalid")

applyDraftDataToDocument :: (Kontrakcja m, DocumentMonad m) =>  Document -> Actor -> m ()
applyDraftDataToDocument draft actor = do
    checkDraftTimeZoneName draft
    unlessM (isPreparation <$> theDocument) $ do
      theDocument >>= \doc -> logAttention "API V2 - Document is not in preparation" $ object [
                                  "document_id" .= (show $ documentid doc)
                                , "document_status" .= (show $ documentstatus doc)
                                ]
      apiError $ serverError "Could not apply draft data to document as document is not in preparation."
    _ <- theDocument >>= \doc -> dbUpdate $ UpdateDraft doc{
                                  documenttitle = documenttitle draft
                                , documentinvitetext = documentinvitetext draft
                                , documentconfirmtext = documentconfirmtext draft
                                , documentdaystosign = documentdaystosign draft
                                , documentdaystoremind = documentdaystoremind draft
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
              sefShouldBeFilledBySender = False
            }]
            replaceEmail a ((SignatoryEmailField ef):fs) = ((SignatoryEmailField $ ef {sefValue = a}):fs)
            replaceEmail a (f:fs) = f:(replaceEmail a fs)

          in case (sigs, nsigs) of
               (asig:_, nasig : nsigs') -> Just $ (setConstantDetails asig nasig) : nsigs'
               _ -> Nothing

mergeSignatoriesIDs :: [SignatoryLink] ->[SignatoryLink] -> [SignatoryLink]
mergeSignatoriesIDs (s:ss) (ns:nss) = (ns {signatorylinkid = signatorylinkid s}) : (mergeSignatoriesIDs ss nss)
mergeSignatoriesIDs _ ns = ns
