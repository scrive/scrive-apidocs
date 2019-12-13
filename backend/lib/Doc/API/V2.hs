module Doc.API.V2 (
  documentAPIV2
) where

import Happstack.Server.Types
import Happstack.StaticRouting

import Doc.API.V2.Calls
import Kontra
import Routing

documentAPIV2 :: Route (Kontra Response)
documentAPIV2 = dir "documents" $ choice
  [ dir "new" $ hPost $ toK0 $ docApiV2New
  , dir "newfromtemplate" $ hPost $ toK1 $ docApiV2NewFromTemplate
  , dir "list" $ hGet $ toK0 $ docApiV2List
  , param $ dir "get" $ hGet $ toK1 $ docApiV2Get
  , param $ dir "getbyshortid" $ hGet $ toK1 $ docApiV2GetByShortID
  , param $ dir "history" $ hGet $ toK1 $ docApiV2History
  , param $ dir "evidenceattachments" $ hGet $ toK1 $ docApiV2EvidenceAttachments
  , param $ dir "update" $ hPost $ toK1 $ docApiV2Update
  , param $ dir "setfile" $ hPost $ toK1 $ docApiV2SetFile
  , param $ dir "removepages" $ hPost $ toK1 $ docApiV2RemovePages
  , param $ dir "start" $ hPost $ toK1 $ docApiV2Start
  , dir "startwithportal" $ hPost $ toK0 $ docApiV2StartWithPortal
  , param $ dir "prolong" $ hPost $ toK1 $ docApiV2Prolong
  , param $ dir "cancel" $ hPost $ toK1 $ docApiV2Cancel
  , param $ dir "trash" $ hPost $ toK1 $ docApiV2Trash
  , dir "trash" $ hPost $ toK0 $ docApiV2TrashMultiple
  , param $ dir "delete" $ hPost $ toK1 $ docApiV2Delete
  , dir "delete" $ hPost $ toK0 $ docApiV2DeleteMultiple
  , param $ dir "remind" $ hPost $ toK1 $ docApiV2Remind
  , dir "remindwithportal" $ hPost $ toK0 $ docApiV2RemindWithPortal
  , param $ dir "forward" $ hPost $ toK1 $ docApiV2Forward
  , param $ dir "setattachments" $ hPost $ toK1 $ docApiV2SetAttachments
  , param $ dir "setautoreminder" $ hPost $ toK1 $ docApiV2SetAutoReminder
  , param $ dir "clone" $ hPost $ toK1 $ docApiV2Clone
  , param $ dir "restart" $ hPost $ toK1 $ docApiV2Restart
  , param $ dir "callback" $ hPost $ toK1 $ docApiV2Callback
  , param $ dir "addimage" $ hPost $ toK1 $ docApiV2AddImage
  , param $ dir "addevidenceevent" $ hPost $ toK1 $ docApiV2AddEvidenceEvent
  , dir "templates" $ dir "setsharing" $ hPost $ toK0 $ docApiV2SetSharing
  , param $ param $ dir "signingdata" $ hGet $ toK2 $ docApiV2SigningData
  , param $ dir "files" $ dir "main" $ hGet $ toK2 $ \docID (_ :: String) ->
    docApiV2FilesMain docID
  , param $ dir "files" $ dir "main" $ hGet $ toK1 $ docApiV2FilesMain
  , param $ dir "files" $ dir "zip" $ hGet $ toK2 $ \docID (_ :: String) ->
    docApiV2FilesFull docID
  , param $ dir "files" $ dir "zip" $ hGet $ toK1 $ docApiV2FilesFull
  , param $ dir "files" $ hGet $ toK3 $ \docID fileID fname ->
    docApiV2FilesGet docID fileID (Just fname)
  , param $ dir "files" $ hGet $ toK2 $ \docID fileID ->
    docApiV2FilesGet docID fileID Nothing
  , param $ dir "files" $ param $ dir "pagescount" $ hGet $ toK2 $ docApiV2FilesPagesCount
  , param $ dir "files" $ param $ dir "page" $ hGet $ toK3 $ docApiV2FilesPage
  , param $ dir "shareablelink" $ choice
    [ dir "generate" $ hPost $ toK1 $ docApiV2GenerateShareableLink
    , dir "discard" $ hPost $ toK1 $ docApiV2DiscardShareableLink
    ]
  , param $ param $ dir "getqrcode" $ hGet $ toK2 $ docApiV2GetQRCode
  , param $ param $ dir "sign" $ hPost $ toK2 $ docApiV2SigSign
  , param $ param $ dir "approve" $ hPost $ toK2 $ docApiV2SigApprove
  , param $ param $ dir "check" $ hPost $ toK2 $ docApiV2SigCheck
  , (param . param . dir "setauthenticationtoview" . hPost . toK2)
    docApiV2SigSetAuthenticationToView
  , (param . param . dir "setauthenticationtoviewarchived" . hPost . toK2)
    docApiV2SigSetAuthenticationToViewArchived
  , (param . param . dir "setauthenticationtosign" . hPost . toK2)
    docApiV2SigSetAuthenticationToSign
  , (param . param . dir "changeemailandmobile" . hPost . toK2)
    docApiV2SigChangeEmailAndMobile
  , param $ param $ dir "reject" $ hPost $ toK2 $ docApiV2SigReject
  , param $ param $ dir "forwardsigning" $ hPost $ toK2 $ docApiV2SigForwardSigning
  , param $ param $ dir "sendsmspin" $ hPost $ toK2 $ docApiV2SigSendSmsPinToSign
  , param $ param $ dir "sendsmspintoview" $ hPost $ toK2 $ docApiV2SigSendSmsPinToView
  , (param . param . dir "smspinidentifytoview" . hPost . toK2)
    docApiV2SigIdentifyToViewWithSmsPin
  , param $ param $ dir "setattachment" $ hPost $ toK2 $ docApiV2SigSetAttachment
  , (param . param . dir "signing" . dir "check" . hGet . toK2)
    docApiV2SigSigningStatusCheck
  , param $ param $ dir "signing" $ dir "cancel" $ hPost $ toK2 $ docApiV2SigSigningCancel
  , (param . param . dir "signing" . dir "highlight" . hPost . toK2)
    docApiV2SetHighlightForPage
  ]
