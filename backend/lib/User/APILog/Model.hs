{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module User.APILog.Model (
    CallLogData(..)
  , CallLogID(..)
  , CallLogItem(..)
  , CallLogRequest(..)
  , CallLogResponse(..)
  , CallLogParam(..)
  , fromCallLogID
  , GetCallLogItem(..)
  , GetCallLogList(..)
  , CreateCallLogItem(..)
  , unjsonCallLogItem
  , unjsonCallLogListForAPI
  ) where

import Control.Monad.Catch
import Data.Binary as B
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Log.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import DB
import Log.Identifier
import User.UserID

data CallLogData = CallLogData
  { cldRequest :: !CallLogRequest
  , cldResponse :: !CallLogResponse
  } deriving (Show, Eq)

data CallLogRequest = CallLogRequest
  { clrqURI :: !String
  , clrqMethod :: !String
  , clrqParamsGet :: ![CallLogParam]
  , clrqParamsPost :: ![CallLogParam]
  } deriving (Show, Eq)

data CallLogResponse = CallLogResponse
  { clrsCode :: !Int32
  , clrsBody :: !String
  } deriving (Show, Eq)

data CallLogParam = CallLogParam
  { clpName :: !String
  , clpValue :: !String
  } deriving (Show, Eq)


unjsonCallLogData :: UnjsonDef CallLogData
unjsonCallLogData =
  objectOf
    $   CallLogData
    <$> fieldBy "request"  cldRequest  "Call Log Request Data"  unjsonCallLogRequest
    <*> fieldBy "response" cldResponse "Call Log Response Data" unjsonCallLogResponse

unjsonCallLogRequest :: UnjsonDef CallLogRequest
unjsonCallLogRequest =
  objectOf
    $   CallLogRequest
    <$> field "uri"    clrqURI    "Call Log Request URI"
    <*> field "method" clrqMethod "Call Log Request Method"
    <*> fieldBy "params_get"
                clrqParamsGet
                "Call Log Request GET Params"
                (arrayOf unjsonCallLogParam)
    <*> fieldBy "params_post"
                clrqParamsPost
                "Call Log Request POST Params"
                (arrayOf unjsonCallLogParam)

{-# ANN unjsonCallLogResponse ("HLint: ignore Redundant bracket" :: String) #-}
unjsonCallLogResponse :: UnjsonDef CallLogResponse
unjsonCallLogResponse =
  objectOf
    $   CallLogResponse
    <$> (field "code" clrsCode "Call Log Response Code")
    <*> (field "body" clrsBody "Call Log Response Body")

unjsonCallLogParam :: UnjsonDef CallLogParam
unjsonCallLogParam =
  objectOf $ CallLogParam <$> field "name" clpName "Call Log Param Name" <*> field
    "value"
    clpValue
    "Call Log Param Value"

newtype CallLogID = CallLogID Int64
  deriving (Eq, Ord)
deriving newtype instance Read CallLogID
deriving newtype instance Show CallLogID

instance PQFormat CallLogID where
  pqFormat = pqFormat @Int64

instance Identifier CallLogID where
  idDefaultLabel = "document_id"
  idValue        = int64AsStringIdentifier . fromCallLogID

instance FromReqURI CallLogID where
  fromReqURI = maybeRead . T.pack

instance Unjson CallLogID where
  unjsonDef = unjsonInvmapR
    (maybe (fail "Can't parse CallLogID") return . maybeRead . T.pack)
    show
    unjsonDef

instance Binary CallLogID where
  put (CallLogID did) = put did
  get = fmap CallLogID B.get

instance FromSQL CallLogID where
  type PQBase CallLogID = PQBase Int64
  fromSQL mbase = CallLogID <$> fromSQL mbase

instance ToSQL CallLogID where
  type PQDest CallLogID = PQDest Int64
  toSQL (CallLogID n) = toSQL n

fromCallLogID :: CallLogID -> Int64
fromCallLogID (CallLogID did) = did


instance PQFormat [CallLogParam] where
  pqFormat = pqFormat @(JSON BS.ByteString)

instance FromSQL [CallLogParam] where
  type PQBase [CallLogParam] = PQBase (JSON BS.ByteString)
  fromSQL mbase = do
    JSON s <- fromSQL mbase
    case Aeson.eitherDecode s of
      Left  _  -> hpqTypesError "fromSQL ([CallLogParam]): can't parse json"
      Right ae -> case parse (arrayOf unjsonCallLogParam) ae of
        (Result res []) -> return res
        (Result _ _) ->
          hpqTypesError "fromSQL ([CallLogParam]): can't parse CallLogParam"

instance ToSQL [CallLogParam] where
  type PQDest [CallLogParam] = PQDest (JSON BS.ByteString)
  toSQL s = toSQL
    (unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True })
                             (arrayOf unjsonCallLogParam)
                             s
    )


data CallLogItem = CallLogItem
  { cliID :: !CallLogID
  , cliUserID :: !UserID
  , cliTime :: !UTCTime
  , cliData :: !CallLogData
  } deriving (Eq, Show)

unjsonCallLogItem :: UnjsonDef CallLogItem
unjsonCallLogItem =
  objectOf
    $   CallLogItem
    <$> field "id"      cliID     "ID of this log item"
    <*> field "user_id" cliUserID "Time of this log item"
    <*> field "time"    cliTime   "Time of this log item"
    <*> fieldBy "data" cliData "All data, which were logged" unjsonCallLogData

unjsonCallLogListForAPI :: UnjsonDef [CallLogItem]
unjsonCallLogListForAPI =
  objectOf $ fieldBy "call_logs" identity "List of log items" (arrayOf unjsonCallLogItem)

selectCallLogItemSelectorsList :: [SQL]
selectCallLogItemSelectorsList =
  [ "id"
  , "user_id"
  , "time"
  , "request_uri"
  , "request_method"
  , "request_params_get"
  , "request_params_post"
  , "response_code"
  , "response_body"
  ]

newtype GetCallLogItem = GetCallLogItem CallLogID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCallLogItem CallLogItem where
  query (GetCallLogItem clid) = do
    runQuery_ . sqlSelect "api_call_logs" $ do
      mapM_ sqlResult selectCallLogItemSelectorsList
      sqlWhereEq "id" clid
    fetchOne fetchCallLogItem

newtype GetCallLogList = GetCallLogList UserID
instance MonadDB m => DBQuery m GetCallLogList [CallLogItem] where
  query (GetCallLogList userid) = do
    runQuery_ . sqlSelect "api_call_logs" $ do
      mapM_ sqlResult selectCallLogItemSelectorsList
      sqlWhereEq "user_id" userid
      sqlOrderBy "time DESC"
      sqlOrderBy "id DESC"
      sqlLimit 100
    fetchMany fetchCallLogItem

data CreateCallLogItem = CreateCallLogItem UserID CallLogData
instance (MonadDB m, MonadTime m, MonadThrow m) => DBUpdate m CreateCallLogItem CallLogItem where
  update (CreateCallLogItem userid cld) = do
    now <- currentTime
    runQuery_ . sqlInsert "api_call_logs" $ do
      sqlSet "user_id" userid
      sqlSet "time"    now
      sqlSet "request_uri" . clrqURI . cldRequest $ cld
      sqlSet "request_method" . clrqMethod . cldRequest $ cld
      sqlSet "request_params_get" . clrqParamsGet . cldRequest $ cld
      sqlSet "request_params_post" . clrqParamsPost . cldRequest $ cld
      sqlSet "response_code" . clrsCode . cldResponse $ cld
      sqlSet "response_body"
        . BS.unpack
        . B64.encode
        . BS.pack
        . clrsBody
        . cldResponse
        $ cld
      mapM_ sqlResult selectCallLogItemSelectorsList
    cli <- fetchOne fetchCallLogItem
    runQuery_ . sqlDelete "api_call_logs" $ do
      sqlWith "times_to_keep" . sqlSelect "api_call_logs" $ do
        sqlWhereEq "user_id" userid
        sqlResult "time"
        sqlLimit 100
        sqlOrderBy "time DESC"
      sqlWhere "time < (SELECT MIN(time) from times_to_keep)"
      sqlWhereEq "user_id" userid
    return cli

fetchCallLogItem
  :: ( CallLogID
     , UserID
     , UTCTime
     , String
     , String
     , [CallLogParam]
     , [CallLogParam]
     , Int32
     , String
     )
  -> CallLogItem
fetchCallLogItem (clid, userid, time, rq_uri, rq_method, rq_params_get, rq_params_post, rs_code, rs_body)
  = CallLogItem
    { cliID     = clid
    , cliUserID = userid
    , cliTime   = time
    , cliData   =
      CallLogData
        { cldRequest  = CallLogRequest { clrqURI        = rq_uri
                                       , clrqMethod     = rq_method
                                       , clrqParamsGet  = rq_params_get
                                       , clrqParamsPost = rq_params_post
                                       }
        , cldResponse =
          CallLogResponse
            { clrsCode = rs_code
            , clrsBody =
              either (const "<DECODING ERROR>") BS.unpack . B64.decode $ BS.pack rs_body
            }
        }
    }
