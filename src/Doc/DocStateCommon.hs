{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Doc.DocStateCommon
where

--import API.Service.Model
--import Company.Model
--import Control.Monad
--import Control.Monad.Reader (ask)
--import Database.HDBC
--import Data.Maybe
--import Data.Word
--import DB.Classes
import DB.Types
--import DB.Utils
import Doc.DocProcess
import Doc.DocStateData
--import Doc.DocStateUtils
--import Doc.DocUtils
--import Happstack.Data.IxSet as IxSet hiding (null)
--import Happstack.State
import Mails.MailsUtil
import MinutesTime
import Misc
import User.Model
import qualified Data.ByteString.Char8 as BS
--import qualified Data.ByteString.UTF8 as BS
--import Util.SignatoryLinkUtils
--import Util.HasSomeCompanyInfo
--import Util.HasSomeUserInfo
--import InputValidation
--import Control.Applicative
--import Doc.DocInfo
--import Data.List
--import File.FileID
--import qualified AppLogger as Log
--import qualified Doc.Model as D
--import qualified Doc.Tables as D

signLinkFromDetails' :: SignatoryDetails
                     -> [SignatoryRole]
                     -> SignatoryLinkID
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' details roles linkid magichash = 
  SignatoryLink { signatorylinkid = linkid
                , signatorydetails = details
                , signatorymagichash = magichash
                , maybesignatory = Nothing
                , maybesupervisor = Nothing  -- This field is now deprecated should use maybecompany instead
                , maybecompany = Nothing
                , maybesigninfo  = Nothing
                , maybeseeninfo  = Nothing
                , maybereadinvite = Nothing
                , invitationdeliverystatus = Unknown
                , signatorysignatureinfo = Nothing
                , signatoryroles = roles
                , signatorylinkdeleted = False
                , signatorylinkreallydeleted = False
                }

{- |
    A blank document containing default values that need to be set before
    saving.
-}
blankDocument :: Document
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentfunctionality        = BasicFunctionality
          , documentctime                = fromSeconds 0 
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = BS.empty
          , documentsealedfiles          = []
          -- , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcsvupload            = Nothing
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          , documentsignatoryattachments = []
          -- , documentattachments          = []
          , documentregion               = defaultValue
          }


{- |
    Determines whether a new document should have either Advanced or Basic
    functionality according to the document's type and the user's preferences.
-}
newDocumentFunctionality :: DocumentType -> User -> DocumentFunctionality
newDocumentFunctionality documenttype user =
  case (getValueForProcess documenttype processbasicavailable, 
        preferreddesignmode $ usersettings user) of
    (Just True, Nothing) -> BasicFunctionality
    (Just True, Just BasicMode) -> BasicFunctionality
    _ -> AdvancedFunctionality
