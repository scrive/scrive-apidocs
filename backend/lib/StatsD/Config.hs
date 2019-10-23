module StatsD.Config where

import Data.Unjson

data StatsDConf = StatsDConf {
    statsdHost    :: String
  , statsdPort    :: Int
  , statsdPrefix  :: String
  } deriving (Eq, Show, Ord)

unjsonStatsDConf :: UnjsonDef StatsDConf
unjsonStatsDConf =
  objectOf
    $   pure StatsDConf
    <*> field "host"   statsdHost   "StatsD host"
    <*> field "port"   statsdPort   "StatsD server port"
    <*> field "prefix" statsdPrefix "StatsD tenant ID"

instance Unjson StatsDConf where
  unjsonDef = unjsonStatsDConf
