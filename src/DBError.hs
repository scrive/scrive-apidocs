-----------------------------------------------------------------------------
-- |
-- Module      :  DBError
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- This module exports DBError, which catalogs the types of errors returned
-- from a DB query or update.
-----------------------------------------------------------------------------

module DBError
    ( DBError (..)
    ) where

{- |
   The different types of errors that could occur when accessing the database.

   Note that this type is purposefully lacking more detailed constructors as
   well as custom error messages. The reason for this is that we do not want
   to "leak" information during a database error. For instance, if a document
   exists for a given DocumentID but the current user does not have access,
   we need to hide that the document exists to prevent information leak.
   Therefore we return DBResourceNotAvailable in both cases.

-}
data DBError = DBResourceNotAvailable -- ^ The queried for resource does not exist OR the current user does not have access
             | DBNotLoggedIn -- ^ There is no user logged in (in Context) and access control requires log in
             | DBDatabaseNotAvailable String -- ^ A generalized error for any problem with the database itself
             deriving (Show, Eq)
