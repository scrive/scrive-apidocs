{-# LANGUAGE ExtendedDefaultRules #-}
module Archive.View
       (
         flashMessageAttachmentArchiveDone,
         flashMessageSignableArchiveDone,
         flashMessageTemplateArchiveDone,
         pageArchive,
         docForListJSON,
         docForListCSV,
         docForListCSVHeader
       )
       where

import Doc.DocStateData
import FlashMessage
import KontraLink
import Text.StringTemplates.Templates
import User.Model

import Control.Applicative
import Data.Maybe
import Data.List
import MinutesTime
import Control.Logic
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Control.Monad.Reader
import Text.JSON


import Doc.DocUtils
import PadQueue.Model
import Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F
import Data.String.Utils (strip)

flashMessageSignableArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageSignableArchiveDone = do
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageDocumentArchiveDone"

flashMessageTemplateArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageTemplateArchiveDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageTemplateArchiveDone"

flashMessageAttachmentArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageAttachmentArchiveDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageAttachmentArchiveDone"

pageArchive :: TemplatesMonad m => User -> MinutesTime -> m String
pageArchive user mt = renderTemplate "pageDocumentsList" $ do
                    F.value "isadmin" $ useriscompanyadmin user && isJust (usercompany user)
                    F.value "month" $ mtMonth mt
                    F.value "year" $ mtYear mt

docForListJSON :: TemplatesMonad m => User -> PadQueue ->  Document -> m JSValue
docForListJSON user padqueue doc = do
  let link = case getSigLinkFor doc user of
        Just sl | not $ isAuthor sl -> LinkSignDoc doc sl
        _                           -> LinkIssueDoc $ documentid doc
      sigFilter sl =   isSignatory sl && (documentstatus doc /= Preparation)
  runJSONGenT $ do
    J.object "fields" $ docFieldsListForJSON (userid user) padqueue doc
    J.objects "subfields" $ map (signatoryFieldsListForJSON padqueue doc) (filter sigFilter (documentsignatorylinks doc))
    J.value "link" $ show link

docFieldsListForJSON :: TemplatesMonad m => UserID -> PadQueue -> Document -> JSONGenT m ()
docFieldsListForJSON userid padqueue doc = do
    J.value "id" $ show $ documentid doc
    J.value "title" $ documenttitle doc
    J.value "status" $ show $ documentstatusclass doc
    J.value "state"  $ show $ documentstatus doc
    J.value "party" $ intercalate ", " $ map getSmartName $ getSignatoryPartnerLinks doc
    J.value "partner" $ intercalate ", " $ map getSmartName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "partnercomp" $ intercalate ", " $ map getCompanyName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "author" $ intercalate ", " $ map getSmartName $ filter isAuthor $ (documentsignatorylinks doc)
    J.value "time" $ formatMinutesTimeRealISO (documentmtime doc)
    J.value "ctime" $ formatMinutesTimeRealISO (documentctime doc)
    J.value "timeouttime" $ formatMinutesTimeRealISO <$> unTimeoutTime <$> documenttimeouttime doc
    J.value "template" $ isTemplate doc
    J.value "partiescount" $ length $ (documentsignatorylinks doc)
    J.value "type" $ case documenttype doc of
                        Template _ -> "template"
                        Signable _ -> "signable"
    J.value "process" $ case toDocumentProcess (documenttype doc) of
                          Contract -> "contract"
                          Offer    -> "offer"
                          Order    -> "order"
    J.value "authentication" $ case nub (map signatorylinkauthenticationmethod (documentsignatorylinks doc)) of
      [StandardAuthentication] -> "standard"
      [ELegAuthentication]     -> "eleg"
      _                        -> "mixed"
    J.value "delivery" $ case nub (map signatorylinkdeliverymethod (documentsignatorylinks doc)) of
      [EmailDelivery] -> "email"
      [PadDelivery]   -> "pad"
      [APIDelivery]   -> "api"
      [MobileDelivery]-> "mobile"
      _                        -> "mixed"
    J.value "anyinvitationundelivered" $ anyInvitationUndelivered  doc && Pending == documentstatus doc
    J.value "shared" $ documentsharing doc == Shared
    J.value "file" $ show <$> (documentsealedfile doc `mplus` documentfile doc)
    J.value "inpadqueue" $  (fmap fst padqueue == Just (documentid doc))
    J.value "deleted" $ documentDeletedForUser doc userid
    J.value "reallydeleted" $ documentReallyDeletedForUser doc userid
    J.value "canperformsigning" $ userCanPerformSigningAction userid doc
    J.value "isviewedbyauthor" $ isSigLinkFor userid (getAuthorSigLink doc)
    J.value "objectversion" $ documentobjectversion doc

signatoryFieldsListForJSON :: TemplatesMonad m => PadQueue -> Document -> SignatoryLink -> JSONGenT m ()
signatoryFieldsListForJSON padqueue doc sl = do
    J.value "id" $ show $ signatorylinkid sl
    J.value "status" $ show $ signatorylinkstatusclass sl
    J.value "name" $ case strip (getCompanyName sl) of
                       "" -> getSmartName sl
                       _  -> getSmartName sl ++ " (" ++ getCompanyName sl ++ ")"
    J.value "time" $ fromMaybe "" $ formatMinutesTimeRealISO <$> (sign `mplus` reject `mplus` seen `mplus` open)
    J.value "invitationundelivered" $ isUndelivered sl && Pending == documentstatus doc
    J.value "inpadqueue" $  (fmap fst padqueue == Just (documentid doc)) && (fmap snd padqueue == Just (signatorylinkid sl))
    J.value "isauthor" $ isAuthor sl
    J.value "authentication" $ case signatorylinkauthenticationmethod sl of
      StandardAuthentication -> "standard"
      ELegAuthentication  -> "eleg"
    J.value "delivery" $ signatorylinkdeliverymethod sl
    where
        sign = signtime <$> maybesigninfo sl
        seen = signtime <$> maybesigninfo sl
        reject = signatorylinkrejectiontime sl
        open = maybereadinvite sl

docForListCSV::  Int -> Document -> [[String]]
docForListCSV agr doc = map (signatoryForListCSV agr doc) $ [Nothing] <| null interestingLinks |> map Just interestingLinks
    where interestingLinks = filter (\x-> isSignatory x && getSmartName x /= "") (documentsignatorylinks doc)

signatoryForListCSV::  Int -> Document -> (Maybe SignatoryLink) -> [String]
signatoryForListCSV _agr doc msl = [
              ("'" ++ show (documentid doc) ++ "'") -- Exel trick
            , documenttitle doc
            , show $ documentstatusclass doc
            , getAuthorName $ doc
            , csvTime $ (documentctime doc)
            , maybe "" csvTime $ signtime <$> documentinvitetime doc
            , maybe "" csvTime $ join $ maybereadinvite <$> msl
            , maybe "" csvTime $ signtime <$> (join $ maybeseeninfo <$> msl)
            , maybe "" csvTime $ signtime <$> (join $ maybesigninfo <$> msl)
            , fromMaybe  "" $ getFullName <$> msl
            , fromMaybe  "" $ getEmail <$> msl
            , fromMaybe  "" $ getPersonalNumber <$> msl
            , fromMaybe  "" $ getCompanyName <$> msl
            , fromMaybe  "" $ getCompanyNumber <$> msl
            ] ++ (map fieldValue $ sortBy fieldNameSort customFieldsOrCheckbox)
    where
        csvTime = formatMinutesTime "%Y-%m-%d %H:%M"
        customFieldsOrCheckbox = filter isCustomOrCheckbox  $ concat $ maybeToList $ signatoryfields <$> signatorydetails <$> msl
        fieldNameSort sf1 sf2 = case (sfType sf1, sfType sf2) of
                                  (CustomFT n1 _, CustomFT n2 _) -> compare n1 n2
                                  (CustomFT _ _,_) -> GT
                                  (SignatureFT n1, SignatureFT n2) -> compare n1 n2
                                  (CheckboxFT n1, CheckboxFT n2) -> compare n1 n2
                                  _ -> EQ
        fieldValue sf = case sfType sf of
                             (CustomFT _ _) -> sfValue sf
                             (CheckboxFT name) -> if (null $ sfValue sf)
                                                     then name ++ " : not checked"
                                                     else name ++ " : checked"
                             _ -> ""
        isCustomOrCheckbox SignatoryField{sfType} = case sfType of
                                            (CustomFT _ _) -> True
                                            (CheckboxFT _) -> True
                                            _ -> False
docForListCSVHeader :: [String]
docForListCSVHeader = [
                          "Id"
                        , "Title"
                        , "Status"
                        , "Author"
                        , "Creation"
                        , "Started"
                        , "Party read invitation"
                        , "Party seen document"
                        , "Party signed document"
                        , "Party name"
                        , "Party mail"
                        , "Party personal number"
                        , "Party company name"
                        , "Party company number"
                       ]
