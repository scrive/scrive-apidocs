module Attachment.API (
    attachmentAPI
  -- only for testing
  , attachmentsApiV2List_
  , attachmentsApiV2Create
  , attachmentsApiV2Download
  , attachmentsApiV2SetSharing
  , attachmentsApiV2Delete
  ) where

import Data.Unjson
import Happstack.Server.Types
import Happstack.StaticRouting
import System.FilePath
import qualified Data.Text as T

import API.V2
import API.V2.Parameters
import AppView
import Attachment.AttachmentID
import Attachment.JSON
import Attachment.Model
import DB
import File.File
import File.Storage
import Kontra
import OAuth.Model
import Routing

attachmentAPI :: Route (Kontra Response)
attachmentAPI =
  dir "api" $ choice [dir "frontend" attachmentAPIV2, dir "v2" attachmentAPIV2]

attachmentAPIV2 :: Route (Kontra Response)
attachmentAPIV2 = dir "attachments" $ choice
  [ dir "list" $ hGet $ toK0 attachmentsApiV2List
  , dir "new" $ hPost $ toK0 attachmentsApiV2Create
  , param $ dir "download" $ hGet $ toK2 $ \attID (_ :: String) ->
    attachmentsApiV2Download attID
  , param $ dir "download" $ hGet $ toK1 attachmentsApiV2Download
  -- bulk operations
  , dir "setsharing" $ hPost $ toK0 attachmentsApiV2SetSharing
  , dir "delete" $ hPost $ toK0 attachmentsApiV2Delete
  ]

attachmentsApiV2List :: Kontrakcja m => m Response
attachmentsApiV2List = api $ do
  attachments <- attachmentsApiV2List_
  let headers = mkHeaders [("Content-Type", "application/json; charset=UTF-8")]
  return $ Ok $ Response 200
                         headers
                         nullRsFlags
                         (unjsonToByteStringLazy unjsonAttachments attachments)
                         Nothing

attachmentsApiV2List_ :: Kontrakcja m => m [Attachment]
attachmentsApiV2List_ = do
  (user, _) <- getAPIUser APIPersonal

  domainStr <- apiV2ParameterOptional $ ApiV2ParameterText "domain"
  mFilters  <- apiV2ParameterOptional
    $ ApiV2ParameterJSON "filter" unjsonAttachmentFiltering
  mSorting <- apiV2ParameterOptional
    $ ApiV2ParameterJSON "sorting" unjsonAttachmentSorting

  let domain = case domainStr of
        Just "All" ->
          [ AttachmentsOfAuthorDeleteValue (user ^. #id) False
          , AttachmentsSharedInUsersUserGroup (user ^. #id)
          ]
        _ -> [AttachmentsOfAuthorDeleteValue (user ^. #id) False]
      filters = fromMaybe [] mFilters
      sorting = fromMaybe [] mSorting

  dbQuery $ GetAttachments domain filters sorting

attachmentsApiV2Create :: Kontrakcja m => m Response
attachmentsApiV2Create = api $ do
  (user, actor) <- getAPIUser APIPersonal
  mTitle        <- apiV2ParameterOptional $ ApiV2ParameterText "title"
  file          <- apiV2ParameterObligatory $ ApiV2ParameterFilePDF "file"

  let title = fromMaybe (takeTextBaseName $ filename file) mTitle
  void $ dbUpdate $ NewAttachment (user ^. #id) title (fileid file) actor

  return $ Created ()
  where
    takeTextBaseName :: Text -> Text
    takeTextBaseName = T.pack . takeBaseName . T.unpack

attachmentsApiV2Download :: Kontrakcja m => AttachmentID -> m Response
attachmentsApiV2Download attid = api $ do
  (user, _) <- getAPIUser APIPersonal
  atts      <- dbQuery $ GetAttachments
    [ AttachmentsSharedInUsersUserGroup (user ^. #id)
    , AttachmentsOfAuthorDeleteValue (user ^. #id) True
    , AttachmentsOfAuthorDeleteValue (user ^. #id) False
    ]
    [AttachmentFilterByID attid]
    []
  case atts of
    [att] -> do
      contents <- getFileIDContents (attachmentfile att)
      return $ Ok $ respondWithPDF False contents
    _ -> apiError $ serverError "Can't access attachment."

attachmentsApiV2SetSharing :: Kontrakcja m => m Response
attachmentsApiV2SetSharing = api $ do
  (user, _) <- getAPIUser APIPersonal
  ids       <- apiV2ParameterObligatory $ ApiV2ParameterJSON "attachment_ids" $ arrayOf
    unjsonDef
  shared <- apiV2ParameterObligatory $ ApiV2ParameterBool "shared"
  dbUpdate $ SetAttachmentsSharing (user ^. #id) ids shared
  return $ Ok ()

attachmentsApiV2Delete :: Kontrakcja m => m Response
attachmentsApiV2Delete = api $ do
  (user, actor) <- getAPIUser APIPersonal
  ids <- apiV2ParameterObligatory $ ApiV2ParameterJSON "attachment_ids" $ arrayOf
    unjsonDef
  dbUpdate $ DeleteAttachments (user ^. #id) ids actor
  return $ Accepted ()
