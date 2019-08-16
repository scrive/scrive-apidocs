module Monitoring ( MonitoringConf(..)
                  , startMonitoringServer
                  ) where

import Data.Unjson
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring.Statsd as EKG

data MonitoringConf = MonitoringConf
  { monitoringHost          :: !Text  -- ^ @statsd@ server hostname or IP address.
  , monitoringPort          :: !Int   -- ^ @statsd@ server port.
  , monitoringFlushInterval :: !Int   -- ^ Data push interval, in ms.
  , monitoringDebug         :: !Bool  -- ^ Print debug output to stderr.
  , monitoringPrefix        :: !Text  -- ^ Prefix to add to all metric names.
  , monitoringSuffix        :: !Text  -- ^ Suffix to add to all metric names.
  } deriving (Eq, Show)

instance Unjson MonitoringConf where
  unjsonDef = objectOf $ MonitoringConf
    <$> fieldDef "host" "127.0.0.1"
        monitoringHost
        "Monitoring server host"
    <*> fieldDef "port" 8125
        monitoringPort
        "Monitoring server port"
    <*> fieldDef "flush_interval" 1000
        monitoringFlushInterval
        "Data push interval, in ms"
    <*> fieldDef "debug" False
        monitoringDebug
        "Print debugging information to stderr"
    <*> fieldDef "prefix" "ekg_monitoring."
        monitoringPrefix
        "Prefix to add to all metric names"
    <*> fieldDef "suffix" ""
        monitoringSuffix
        "Suffix to add to all metric names"

startMonitoringServer :: MonitoringConf -> IO EKG.Statsd
startMonitoringServer MonitoringConf{..} = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  let opts = EKG.StatsdOptions { EKG.host          = monitoringHost
                               , EKG.port          = monitoringPort
                               , EKG.flushInterval = monitoringFlushInterval
                               , EKG.debug         = monitoringDebug
                               , EKG.prefix        = monitoringPrefix
                               , EKG.suffix        = monitoringSuffix
                               }
  EKG.forkStatsd opts store
