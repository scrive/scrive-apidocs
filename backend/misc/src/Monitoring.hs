module Monitoring ( MonitoringConf(..)
                  , startMonitoringServer
                  ) where

import Data.Default
import Data.Text
import Data.Unjson
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring.Statsd as EKG

import KontraPrelude

data MonitoringConf = MonitoringConf
  { monitoringHost          :: !Text  -- ^ @statsd@ server hostname or IP address.
  , monitoringPort          :: !Int   -- ^ @statsd@ server port.
  , monitoringFlushInterval :: !Int   -- ^ Data push interval, in ms.
  , monitoringDebug         :: !Bool  -- ^ Print debug output to stderr.
  , monitoringPrefix        :: !Text  -- ^ Prefix to add to all metric names.
  , monitoringSuffix        :: !Text  -- ^ Suffix to add to all metric names.
  } deriving (Eq, Show)

instance Default MonitoringConf where
  def = MonitoringConf
    { monitoringHost          = "127.0.0.1"
    , monitoringPort          = 8125
    , monitoringFlushInterval = 1000
    , monitoringDebug         = False
    , monitoringPrefix        = "ekg_monitoring."
    , monitoringSuffix        = ""
    }

instance Unjson MonitoringConf where
  unjsonDef = objectOf $ MonitoringConf
    <$> fieldDef "host" (monitoringHost def)
        monitoringHost
        "Monitoring server host"
    <*> fieldDef "port" (monitoringPort def)
        monitoringPort
        "Monitoring server port"
    <*> fieldDef "flush_interval" (monitoringFlushInterval def)
        monitoringFlushInterval
        "Data push interval, in ms"
    <*> fieldDef "debug" (monitoringDebug def)
        monitoringDebug
        "Print debugging information to stderr"
    <*> fieldDef "prefix" (monitoringPrefix def)
        monitoringPrefix
        "Prefix to add to all metric names"
    <*> fieldDef "suffix" (monitoringSuffix def)
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
