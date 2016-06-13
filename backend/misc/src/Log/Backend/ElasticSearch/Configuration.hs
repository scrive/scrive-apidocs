module Log.Backend.ElasticSearch.Configuration (
    ElasticSearchConfig(..)
  ) where

import Data.Default
import Data.Text (Text)
import Data.Unjson

import KontraPrelude

data ElasticSearchConfig = ElasticSearchConfig {
    esServer  :: !Text
  , esIndex   :: !Text
  , esMapping :: !Text
  } deriving (Eq, Show)

instance Unjson ElasticSearchConfig where
  unjsonDef = objectOf $ ElasticSearchConfig
    <$> field "server"
        esServer
        "Server (host:port)"
    <*> field "index"
        esIndex
        "Index"
    <*> field "mapping"
        esMapping
        "Mapping"

instance Default ElasticSearchConfig where
  def = ElasticSearchConfig {
      esServer  = "http://localhost:9200"
    , esIndex   = "kontrakcja"
    , esMapping = "logs"
    }
