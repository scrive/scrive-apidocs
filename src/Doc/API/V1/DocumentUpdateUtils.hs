{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V1.DocumentUpdateUtils (
    applyDraftDataToDocument,
    draftIsChangingDocument
  ) where

import Control.Conditional (whenM, unlessM)
import Control.Exception.Lifted (throwIO)
import Control.Monad
import Data.Functor
import Data.List
import qualified Control.Exception.Lifted as E

import API.Monad (serverError,badInput)
import DB
import DB.TimeZoneName
import Doc.DocInfo (isPreparation)
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument)
import Doc.DocUtils
import Doc.Model
import Doc.SignatoryLinkID
import Kontra
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Monad
import qualified Log

checkDraftTimeZoneName ::  (Kontrakcja m) =>  Document -> m ()
checkDraftTimeZoneName draft = do
  void $  (mkTimeZoneName $ toString (documenttimezonename draft))
    `E.catch` (\(_::E.SomeException) ->  throwIO $ SomeKontraException $ badInput "Timezone name is invalid.")

applyDraftDataToDocument :: (Kontrakcja m, DocumentMonad m) =>  Document -> Actor -> m ()
applyDraftDataToDocument draft actor = do
    checkDraftTimeZoneName draft
    unlessM (isPreparation <$> theDocument) $ do
      theDocument >>= \doc -> Log.attention_ $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      throwIO $ SomeKontraException $ serverError "applyDraftDataToDocument failed"
    _ <- theDocument >>= \doc -> dbUpdate $ UpdateDraft doc{
                                  documenttitle = documenttitle draft
                                , documentinvitetext = documentinvitetext draft
                                , documentconfirmtext = documentconfirmtext draft
                                , documentdaystosign = documentdaystosign draft
                                , documentdaystoremind = documentdaystoremind draft
                                , documentshowheader = documentshowheader draft
                                , documentshowpdfdownload = documentshowpdfdownload draft
                                , documentshowrejectoption = documentshowrejectoption draft
                                , documentshowfooter = documentshowfooter draft
                                , documentlang = documentlang draft
                                , documenttags = documenttags draft
                                , documentapicallbackurl = documentapicallbackurl draft
                                , documenttimezonename = documenttimezonename draft
                                } actor
    -- Only allow transition from 'unsaveddraft: true' to 'unsaveddraft: false'
    whenM ((\doc -> (documentunsaveddraft doc) && not (documentunsaveddraft draft)) <$> theDocument) $ do
         dbUpdate $ SetDocumentUnsavedDraft (documentunsaveddraft draft)
    whenM ((\doc -> isTemplate draft && (not $ isTemplate doc)) <$> theDocument) $ do
         dbUpdate $ TemplateFromDocument actor
    documentauthorattachments <$> theDocument >>= \atts -> forM_ atts $ \att -> do
            when_ (not $ att `elem` (documentauthorattachments draft)) $ do
              dbUpdate $ RemoveDocumentAttachment (authorattachmentfile att) actor
    documentsignatorylinks <$> theDocument >>= \siglinks -> case (mergeAuthorDetails siglinks (sortBy compareSL $ documentsignatorylinks draft)) of
         Nothing   -> throwIO $ SomeKontraException $ serverError "Problem with author details while sending draft"
         Just sigs -> do
           -- Checking if some integrations depend on fact that we don't change fstname and lastname for author. To be removed till 20. II.
           let  (Just oldAuthor) = find isAuthor $ documentsignatorylinks $ draft
           let  (Just newAuthor) = find isAuthor sigs
           when (getFirstName oldAuthor /= getFirstName newAuthor || getLastName oldAuthor /= getLastName newAuthor) $ do
            Log.mixlog_ $ "Checkup: Update could changed author details from " ++ getFullName oldAuthor ++ " to " ++ getFullName newAuthor
           -- End testing

           res <- dbUpdate $ ResetSignatoryDetails (sortBy compareSL $ sigs) actor
           unless res $ throwIO $ SomeKontraException $ serverError "applyDraftDataToDocument failed"


compareSL :: SignatoryLink -> SignatoryLink -> Ordering
compareSL s1 s2 | signatoryisauthor s1 = LT
                | signatoryisauthor s2 = GT
                | signatorylinkid s2 == unsafeSignatoryLinkID 0 = LT
                | signatorylinkid s1 == unsafeSignatoryLinkID 0 = GT
                | otherwise = compare (signatorylinkid s1) (signatorylinkid s2)




mergeAuthorDetails :: [SignatoryLink] ->[SignatoryLink] -> Maybe [SignatoryLink]
mergeAuthorDetails sigs nsigs =
          let
            (nasig', nsigs') = partition isAuthor nsigs
            (asig, _) = partition isAuthor sigs
            setConstantDetails a =  replaceFieldValue FirstNameFT (getFirstName a) .
                                    replaceFieldValue LastNameFT (getLastName a) .
                                    replaceFieldValue EmailFT   (getEmail a) .
                                    (\s -> s {maybesignatory = maybesignatory a}) .  -- We need to be sure that we will not disconnect author
                                    (\s -> s {signatorylinkid = signatorylinkid a})  -- And we try to keep original id of author signatory
          in case (asig, nasig') of
               ([asig'], [nasig'']) -> Just $ (setConstantDetails asig' nasig'') : nsigs'
               _ -> Nothing


-- TODO can't we refactor this to take a list of documenttitle, documentinvitetext etc?
draftIsChangingDocument :: Document -> Document -> Bool
draftIsChangingDocument draft doc =
        (documenttitle draft /= documenttitle doc)
     || (documentinvitetext draft /= documentinvitetext doc)
     || (documentlang draft /= documentlang doc)
     || (documenttags draft /= documenttags doc)
     || (documentapicallbackurl draft /= documentapicallbackurl doc)
     || (isTemplate draft /= isTemplate doc)
     || (documentdaystosign draft /= documentdaystosign doc)
     || (documentdaystoremind draft /= documentdaystoremind doc)
     || (documentshowheader draft /= documentshowheader doc)
     || (documentshowpdfdownload draft /= documentshowpdfdownload doc)
     || (documentshowrejectoption draft /= documentshowrejectoption doc)
     || (documentshowfooter draft /= documentshowfooter doc)
     || (documenttimezonename draft /= documenttimezonename doc)
     || (draftIsChangingDocumentSignatories (documentsignatorylinks draft) (documentsignatorylinks doc))
     || (documentunsaveddraft draft /= documentunsaveddraft doc)

draftIsChangingDocumentSignatories :: [SignatoryLink] -> [SignatoryLink] -> Bool
draftIsChangingDocumentSignatories (sl':sls') (sl:sls) = (newSignatorySignatoryLinkIsChangingSignatoryLink sl' sl) || (draftIsChangingDocumentSignatories sls' sls)
draftIsChangingDocumentSignatories [] [] = False
draftIsChangingDocumentSignatories _ _ = True


newSignatorySignatoryLinkIsChangingSignatoryLink :: SignatoryLink -> SignatoryLink -> Bool
newSignatorySignatoryLinkIsChangingSignatoryLink newsl sl =
        (signatorylinkid newsl /= signatorylinkid sl)
     || (signatoryfields newsl /= signatoryfields sl)
     || (signatoryisauthor newsl /= signatoryisauthor sl)
     || (signatoryispartner newsl /= signatoryispartner sl)
     || (signatorysignorder newsl /= signatorysignorder sl)
     || (signatoryattachments newsl /= signatoryattachments sl)
     || (signatorylinkcsvupload newsl /= signatorylinkcsvupload sl)
     || (signatorylinksignredirecturl newsl /= signatorylinksignredirecturl sl)
     || (signatorylinkauthenticationmethod newsl /= signatorylinkauthenticationmethod sl)
     || (signatorylinkdeliverymethod newsl /= signatorylinkdeliverymethod sl)
