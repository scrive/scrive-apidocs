module SMS.Migrations (
    messengerMigrations
  ) where

import DB

-- Note: ALWAYS append new migrations TO THE END of this list.
messengerMigrations :: MonadDB m => [Migration m]
messengerMigrations = [
  ]
