module Planhat
    (
      module Planhat.Communication
    , module Planhat.Config
    , module Planhat.Reporting
    ) where

import Planhat.Communication
import Planhat.Config
import Planhat.Reporting

-- Planhat terminology
-- *******************
--
-- * 'action': a user did something quantitative (sent document, logged in). We
--   send in a human-friendly string, e.g. "Document sent" and a
--   number. Reported by using the endpoint '/analytics' which is part of the
--   their 'open API'; just a so called 'tenant ID' is needed to send in an
--   update.
--
-- * 'metric': something happened on the company level. This is reported by
--   sending in a `dimensionId` and a `value` for it. `dimensionId` is really a
--   variable name so no spaces are allowed in the string that identifies
--   it. This data is strongly associated with time - a reported dimension will
--   get a timestamp that is the time the request reached Planhat, if none was
--   provided. Reported by using the endpoint '/dimensiondata' which also is
--   part of the 'open API'.
--
-- This is the terminology adopted when naming the helpers for the endpoints
-- (phActionURL and phMetricsURL).
--
