module Doc.API.V2 (
    limitedDocumentAPIV2
  , documentAPIV2
) where

import Happstack.Server.Types
import Happstack.StaticRouting

import Doc.API.V2.Calls
import Kontra
import KontraPrelude
import Routing

-- Subset of v2 limited to list and history call. Rewrite to frontend was splited into two parts, and first part
-- of rewite requires only this calls. Should be removed when documentAPIV2 is connected in main routing table.
limitedDocumentAPIV2 :: Route (Kontra Response)
limitedDocumentAPIV2 = dir "documents" $ choice [
    dir "list" $ hGet $ toK0 $ docApiV2List
  , param $ dir "history" $ hGet $ toK1 $ docApiV2History
  ]

documentAPIV2 ::  Route (Kontra Response)
documentAPIV2  = dir "documents" $ choice [
    dir "new"             $ hPost $ toK0 $ docApiV2New
  , dir "newfromtemplate" $ hPost $ toK1 $ docApiV2NewFromTemplate

  , dir "available" $ hGet $ toK0 $ docApiV2Available
  , dir "list"      $ hGet $ toK0 $ docApiV2List

  , param $ dir "get" $ hGet $ toK1 $ docApiV2Get
  , param $ dir "history"             $ hGet $ toK1 $ docApiV2History
  , param $ dir "evidenceattachments" $ hGet $ toK1 $ docApiV2EvidenceAttachments

  , param $ dir "update" $ hPost $ toK1 $ docApiV2Update
  , param $ dir "start"           $ hPost $ toK1 $ docApiV2Start
  , param $ dir "prolong"         $ hPost $ toK1 $ docApiV2Prolong
  , param $ dir "cancel"          $ hPost $ toK1 $ docApiV2Cancel
  , param $ dir "trash"           $ hPost $ toK1 $ docApiV2Trash
  , param $ dir "delete"          $ hPost $ toK1 $ docApiV2Delete
  , param $ dir "remind"          $ hPost $ toK1 $ docApiV2Remind
  , param $ dir "forward"         $ hPost $ toK1 $ docApiV2Forward
  , param $ dir "setfile"         $ hPost $ toK1 $ docApiV2SetFile
  , param $ dir "setattachments"  $ hPost $ toK1 $ docApiV2SetAttachments
  , param $ dir "setautoreminder" $ hPost $ toK1 $ docApiV2SetAutoReminder
  , param $ dir "clone"           $ hPost $ toK1 $ docApiV2Clone
  , param $ dir "restart"         $ hPost $ toK1 $ docApiV2Restart

  , param $ dir "files" $ dir "main" $ hGet $ toK2 $ docApiV2FilesMain
  , param $ dir "files"              $ hGet $ toK3 $ docApiV2FilesGet

  , param $ dir "texts" $ hGet $ toK2 $ docApiV2Texts

  , param $ param $ dir "setauthenticationtoview" $ hPost $ toK2 $ docApiV2SigSetAuthenticationToView
  , param $ param $ dir "setauthenticationtosign" $ hPost $ toK2 $ docApiV2SigSetAuthenticationToSign
  , param $ param $ dir "reject"                  $ hPost $ toK2 $ docApiV2SigReject
  , param $ param $ dir "check"                   $ hPost $ toK2 $ docApiV2SigCheck
  , param $ param $ dir "sign"                    $ hPost $ toK2 $ docApiV2SigSign
  , param $ param $ dir "sendsmspin"              $ hPost $ toK2 $ docApiV2SigSendSmsPin
  , param $ param $ dir "setattachment"           $ hPost $ toK2 $ docApiV2SigSetAttachment
  ]
