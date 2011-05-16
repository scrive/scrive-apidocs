module DBError 
    ( DBError (..)
    ) where

data DBError = DBResourceNotAvailable 
             | DBNotLoggedIn
             | DBDatabaseNotAvailable String
