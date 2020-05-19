module Doc.AccessControl
  ( maybeAuthenticateSignatory
  , guardDocumentReadAccess
  , guardDocumentActionPermission
  , apiRequireDocPermission
  , accessControlDocCheck
  )
where

import AccessControl.Check
import AccessControl.Types
import API.V2.Errors
import API.V2.MonadUtils
import API.V2.User
import API.V2.Utils
import DB
import Doc.API.V2.DocumentAccess
import Doc.DocInfo
import Doc.SignatoryLinkID (SignatoryLinkID)
import Doc.Tokens.Model
import Doc.Types.Document (Document(..))
import Doc.Types.SignatoryLink
import Kontra
import OAuth.Model
import User.Model
import Util.SignatoryLinkUtils (getSigLinkFor)

guardDocumentReadAccess
  :: Kontrakcja m => Maybe SignatoryLinkID -> Document -> m DocumentAccess
guardDocumentReadAccess mSignatoryId doc = do
  -- Maybe Signatory if request has signatory ID and is authenticated
  -- to be linked to this document
  mSignatory :: Maybe SignatoryLink <- bindMaybeM mSignatoryId
    $ maybeAuthenticateSignatory doc

  -- Maybe User if logged in
  mUser :: Maybe User <- fmap fst <$> getMaybeAPIUser APIDocCheck

  -- Maybe signatory derived if user is associated to one of document signatory
  let mSignatoryFromUser :: Maybe SignatoryLink =
        mUser >>= \user -> getSigLinkFor user doc

      userIsAuthor = maybe False signatoryisauthor mSignatoryFromUser

  -- Maybe user derived if signatory is associated to a user.
  -- The user may have access to a document folder, but can never
  -- access other documents in the folder because the user is derived
  -- only if the signatory is authenticated to this document already.
  mUserFromSignatory :: Maybe User <- bindMaybeM (maybesignatory =<< mSignatory)
                                                 (dbQuery . GetUserByID)

  -- Maybe document access from mSignatory
  let mSignatoryDocumentAccess :: Maybe DocumentAccessMode =
        documentAccessForAuthenticatedSignatory doc =<< mSignatory

  -- Maybe document access from mSignatoryFromUser
  let mSignatoryDocumentAccessFromUser :: Maybe DocumentAccessMode =
        documentAccessForAuthenticatedSignatory doc =<< mSignatoryFromUser

  -- Pick signatory ID from either mSignatory or mSignatoryFromUser
  let mPickedSignatoryId :: Maybe SignatoryLinkID =
        signatorylinkid <$> (mSignatory <|> mSignatoryFromUser)

  -- Maybe document access from mUser
  mUserDocumentAccess :: Maybe DocumentAccessMode <- bindMaybeM mUser $ \user ->
    documentAccessModeForAuthenticatedUser doc user userIsAuthor mPickedSignatoryId

  -- Maybe document access from mUserFromSignatory
  mUserDocumentAccessFromSignatory :: Maybe DocumentAccessMode <-
    bindMaybeM mUserFromSignatory $ \user ->
      documentAccessModeForAuthenticatedUser doc user userIsAuthor mPickedSignatoryId

  {-
    Pick a desired document access to show, prioritize by user > signatory,
    main identity > derived identity.
      - if a valid signatory is provided in the request, use that.
      - else if a valid user is logged in, use that.
      - else if the user is logged in and is a valid signatory, use that.
      - else if the signatory link is valid and is linked to a user, use that.
  -}
  let mPickedDocumentAccess =
        mSignatoryDocumentAccess
          <|> mUserDocumentAccess
          <|> mSignatoryDocumentAccessFromUser
          <|> mUserDocumentAccessFromSignatory

  case mPickedDocumentAccess of
    Just accessMode ->
      -- Return document if request have access, with the picked access mode
      return . documentAccess doc $ accessMode
    Nothing -> case mUser <|> mUserFromSignatory of
      Just _ ->
        -- If there is authenticated user, throw documentActionForbidden
        apiError documentActionForbidden
      Nothing ->
        -- Otherwise for signatory or unauthenticated session, invalidAuthorization
        apiError invalidAuthorization
  where
    bindMaybeM :: (Monad m) => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
    bindMaybeM mx cont = case mx of
      Just x  -> cont x
      Nothing -> return Nothing

guardDocumentActionPermission :: Kontrakcja m => AccessAction -> User -> Document -> m ()
guardDocumentActionPermission action user doc = do
  requiredPerm <- apiRequireAnyPermission [ canDo action res | res <- docResources doc ]
  apiAccessControlWithError user
                            requiredPerm
                            (apiError documentActionForbidden)
                            (return ())

documentAccessModeForAuthenticatedUser
  :: Kontrakcja m
  => Document
  -> User
  -> Bool
  -> Maybe SignatoryLinkID
  -> m (Maybe DocumentAccessMode)
documentAccessModeForAuthenticatedUser doc user userIsAuthor mSignatoryId = do
  requiredUpdatePerm  <- apiRequireDocPermission UpdateA doc

  hasUpdatePermission <- apiAccessControlCheck user requiredUpdatePerm
  if hasUpdatePermission
    then if userIsAuthor
      -- FIXME: Currently we use AuthorDocumentAccess to preserve legacy behavior of author
      -- having the same document access mode when getting the document right after creating
      -- it. If we want to cahnge it to FolderDocumentAccess by default, many tests
      -- have to be modified on the expected result both after creation and also getting doc.
      then return . Just $ AuthorDocumentAccess
      else return . Just $ FolderDocumentAccess mSignatoryId
    else do
      requiredReadPerm  <- apiRequireDocPermission ReadA doc
      hasReadPermission <- apiAccessControlCheck user requiredReadPerm

      if hasReadPermission
        then case mSignatoryId of
          Just signatoryId -> return . Just $ SignatoryDocumentAccess signatoryId
          Nothing          -> return . Just $ CompanySharedDocumentAccess
        else return Nothing

documentAccessForAuthenticatedSignatory
  :: Document -> SignatoryLink -> Maybe DocumentAccessMode
documentAccessForAuthenticatedSignatory doc signatory = if isAccessibleBySignatories doc
  then Just . SignatoryDocumentAccess $ signatorylinkid signatory
  else Nothing

maybeAuthenticateSignatory
  :: Kontrakcja m => Document -> SignatoryLinkID -> m (Maybe SignatoryLink)
maybeAuthenticateSignatory doc slid = do
  sid          <- view #sessionID <$> getContext
  validSession <- dbQuery $ CheckDocumentSession sid slid
  if validSession
    then fmap Just . apiGuardJust (documentNotFound (documentid doc)) $ getSigLinkFor
      slid
      doc
    else return Nothing

apiRequireDocPermission
  :: Kontrakcja m => AccessAction -> Document -> m PermissionCondition
apiRequireDocPermission action doc =
  apiRequireAnyPermission [ canDo action res | res <- docResources doc ]
