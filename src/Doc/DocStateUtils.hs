{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStateUtils
-- Maintainer  :  
-- Stability   :  development
-- Portability :  portable
--
-- This module provides  low operations modify document interface and some utils for working with documents
-- Reasons for that: 1) growing number of operations in DocState 
--                   2) Stopping devs from iusing modify function since there is a split to templates and docs 
--                      and some updates make no sense on templates.
-----------------------------------------------------------------------------

module Doc.DocStateUtils

where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans
import User.UserState
import Happstack.Data.IxSet as IxSet
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Debug.Trace
import Misc
import Control.Monad
import Data.List (find)
import MinutesTime
import Data.List (zipWith4,partition)
import System.Random
import Data.Word
import Data.Int
import System.Log.Logger (errorM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Mails.MailsUtil
import Data.Data (Data)
import qualified Data.Generics.SYB.WithClass.Derive as SYB
import Doc.DocStateData


-- DB UPDATE UTILS
insertNewDocument doc = 
   do 
    documents <- ask
    docid <- getUnique64 documents DocumentID
    let docWithId = doc {documentid = docid}
    modify $ insert docWithId
    return docWithId


modifyDocument :: DocumentID 
               -> (Document -> Either String Document) 
               -> Update Documents (Either String Document)
modifyDocument docid action = modifyDocument' docid (return . action)
                
modifyDocument' :: DocumentID 
               -> (Document ->  Update Documents (Either String Document)) 
               -> Update Documents (Either String Document)
modifyDocument' docid action = do
  documents <- ask
  case getOne (documents @= docid) of
    Nothing -> return $ Left "no such document"
    Just document -> 
        do
        actionresult <- action document
        case actionresult of
          Left message -> return $ Left message
          Right newdocument -> 
              do
                when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                modify (updateIx docid newdocument)
                return $ Right newdocument


-- OTHER UTILS

signatoryname :: SignatoryDetails -> BS.ByteString
signatoryname s = 
    if (BS.null $ signatorysndname s) 
        then (signatoryfstname s) 
        else (signatoryfstname s) `BS.append` (BS.fromString " ") `BS.append` (signatorysndname s)

       
isMatchingSignatoryLink :: User -> SignatoryLink -> Bool
isMatchingSignatoryLink user sigLink = signatoryMatches || emailMatches
  where 
  signatoryMatches = maybe False (\s -> unSignatory s == userid user)  (maybesignatory sigLink)
  emailMatches = (signatoryemail . signatorydetails $ sigLink) == (unEmail . useremail $ userinfo user)
  
findMaybeUserByEmail [] _ = Nothing
findMaybeUserByEmail ((email, user):eus) em 
    | email == em = Just user
    | otherwise   = findMaybeUserByEmail eus em

{- |
   Is the user an author of the document. We may want to ask the same if we just have userid or signatorylink
 -}
class MayBeAuthor a where
  isAuthor :: Document -> a -> Bool

instance MayBeAuthor User where
  isAuthor d u = isAuthor d $ userid u
  
instance MayBeAuthor UserID where
  isAuthor d uid = uid == (unAuthor . documentauthor $ d)   
  
instance MayBeAuthor SignatoryLink where
  isAuthor d sl = case maybesignatory sl of
                   Just s -> unSignatory s == ( unAuthor . documentauthor $ d)
                   Nothing -> False

instance (MayBeAuthor a) => MayBeAuthor (Maybe a) where
  isAuthor d (Just s) = isAuthor d s
  isAuthor _ _        = False


anyInvitationUndelivered =  not . Prelude.null . undeliveredSignatoryLinks
undeliveredSignatoryLinks doc =  filter ((== Undelivered) . invitationdeliverystatus) $ documentsignatorylinks doc

{- |
   Build a SignatoryDetails from a User with no fields
 -}
signatoryDetailsFromUser user = 
    SignatoryDetails { signatoryfstname = userfstname $ userinfo user 
                     , signatorysndname = usersndname $ userinfo user 
                     , signatoryemail = unEmail $ useremail $ userinfo user
                     , signatorycompany = usercompanyname $ userinfo user
                     , signatorynumber = usercompanynumber $ userinfo user
                     , signatoryfstnameplacements = []
                     , signatorysndnameplacements = []
                     , signatorycompanyplacements = []
                     , signatoryemailplacements = []
                     , signatorynumberplacements = []
                     , signatoryotherfields = []
                     }
