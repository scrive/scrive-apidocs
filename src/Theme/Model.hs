{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Theme.Model (
      ThemeID
    , Theme(..)
    , GetTheme(..)
    , GetThemesForDomain(..)
    , GetThemesForCompany(..)
    , GetThemesMD5(..)
    , UpdateThemeForDomain(..)
    , UpdateThemeForCompany(..)
    , InsertNewThemeForCompany(..)
    , InsertNewThemeForDomain(..)
    , MakeThemeOwnedByCompany(..)
    , MakeThemeOwnedByDomain(..)
    , UnsafeInsertNewThemeWithoutOwner(..)
    , DeleteThemeOwnedByDomain(..)
    , DeleteThemeOwnedByCompany(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import BrandedDomain.BrandedDomainID
import Company.CompanyID
import DB
import OurPrelude
import Theme.ThemeID

data Theme = Theme {
    themeID                       :: !ThemeID
  , themeName                     :: !String
  , themeLogo                     :: !(Binary BS.ByteString)
  , themeBrandColor               :: !String
  , themeBrandTextColor           :: !String
  , themeActionColor              :: !String
  , themeActionTextColor          :: !String
  , themeActionSecondaryColor     :: !String
  , themeActionSecondaryTextColor :: !String
  , themePositiveColor            :: !String
  , themePositiveTextColor        :: !String
  , themeNegativeColor            :: !String
  , themeNegativeTextColor        :: !String
  , themeFont                     :: !String
} deriving (Eq, Ord, Show) 

data GetTheme = GetTheme ThemeID
instance (MonadDB m,MonadThrow m) => DBQuery m GetTheme Theme where
  query (GetTheme tid) = do
    runQuery_ . sqlSelect "themes" $ do
      sqlWhereEq "id" tid
      selectThemesSelectors
    fetchOne fetchTheme

data GetThemesForDomain = GetThemesForDomain BrandedDomainID
instance (MonadDB m,MonadThrow m) => DBQuery m GetThemesForDomain [Theme] where
  query (GetThemesForDomain did) = do
    runQuery_ . sqlSelect "themes, theme_owners as o" $ do
      sqlWhere "id = o.theme_id"
      sqlWhereEq "o.domain_id" did
      selectThemesSelectors
    fetchMany fetchTheme


data GetThemesForCompany = GetThemesForCompany CompanyID
instance (MonadDB m,MonadThrow m) => DBQuery m GetThemesForCompany [Theme] where
  query (GetThemesForCompany cid) = do
    runQuery_ . sqlSelect "themes, theme_owners as o" $ do
      sqlWhere "id = o.theme_id"
      sqlWhereEq "o.company_id" cid
      selectThemesSelectors
    fetchMany fetchTheme

data GetThemesMD5 = GetThemesMD5 [ThemeID]
instance (MonadDB m,MonadThrow m) => DBQuery m GetThemesMD5 [String] where
  query (GetThemesMD5 tids) = do
    runQuery_ . sqlSelect "themes" $ do
      sqlWhereIn "id" tids
      sqlOrderBy "id"
      selectThemesMD5
    fetchMany unSingle

data UpdateThemeForCompany = UpdateThemeForCompany CompanyID Theme
instance (MonadDB m,MonadThrow m) => DBUpdate m UpdateThemeForCompany Bool where
  update (UpdateThemeForCompany cid t) = do
    runQuery01 . sqlUpdate "themes" $ do
      setThemeData t
      sqlWhereEq "id" $ themeID t
      sqlWhereInSql "id" $ do
        sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" $ cid
          sqlWhereEq "theme_id" $ themeID t
          sqlResult "theme_id"

data UpdateThemeForDomain = UpdateThemeForDomain BrandedDomainID Theme
instance (MonadDB m,MonadThrow m) => DBUpdate m UpdateThemeForDomain Bool where
  update (UpdateThemeForDomain did t) = do
    runQuery01 . sqlUpdate "themes" $ do
      setThemeData t
      sqlWhereEq "id" $ themeID t
      sqlWhereInSql "id" $ do
        sqlSelect "theme_owners" $ do
          sqlWhereEq "domain_id" $ did
          sqlWhereEq "theme_id" $ themeID t
          sqlResult "theme_id"
      sqlWhereNotExists $ do -- Never try to change themes of main domain
        sqlSelect "branded_domains" $ do
          sqlWhereEq "id" $ did
          sqlWhereEq "main_domain" $ True

setThemeData :: Theme -> State SqlUpdate ()
setThemeData t = do
      sqlSet "name" $ themeName t
      sqlSet "logo" $ themeLogo t
      sqlSet "brand_color" $ themeBrandColor t
      sqlSet "brand_text_color" $ themeBrandTextColor t
      sqlSet "action_color" $ themeActionColor t
      sqlSet "action_text_color" $ themeActionTextColor t
      sqlSet "action_secondary_color" $ themeActionSecondaryColor t
      sqlSet "action_secondary_text_color" $ themeActionSecondaryTextColor t
      sqlSet "positive_color" $ themePositiveColor t
      sqlSet "positive_text_color" $ themePositiveTextColor t
      sqlSet "negative_color" $ themeNegativeColor t
      sqlSet "negative_text_color" $ themeNegativeTextColor t
      sqlSet "font" $ themeFont t

data InsertNewThemeForCompany = InsertNewThemeForCompany CompanyID Theme
instance (MonadDB m,MonadThrow m) => DBUpdate m InsertNewThemeForCompany Theme where
  update (InsertNewThemeForCompany cid t) = do
    nt <- dbUpdate $ UnsafeInsertNewThemeWithoutOwner t
    dbUpdate $ MakeThemeOwnedByCompany cid (themeID nt)
    return nt

data InsertNewThemeForDomain = InsertNewThemeForDomain BrandedDomainID Theme
instance (MonadDB m,MonadThrow m) => DBUpdate m InsertNewThemeForDomain Theme where
  update (InsertNewThemeForDomain did t) = do
    nt <- dbUpdate $ UnsafeInsertNewThemeWithoutOwner t
    dbUpdate $ MakeThemeOwnedByDomain did (themeID nt)
    return nt

data MakeThemeOwnedByCompany = MakeThemeOwnedByCompany CompanyID ThemeID
instance (MonadDB m,MonadThrow m) => DBUpdate m MakeThemeOwnedByCompany () where
  update (MakeThemeOwnedByCompany cid tid) = do
    runQuery_ . sqlInsert "theme_owners" $ do
      sqlSet "theme_id"   $ tid
      sqlSet "company_id" $ cid

data MakeThemeOwnedByDomain = MakeThemeOwnedByDomain BrandedDomainID ThemeID
instance (MonadDB m,MonadThrow m) => DBUpdate m MakeThemeOwnedByDomain () where
  update (MakeThemeOwnedByDomain did tid) = do
    runQuery_ . sqlInsert "theme_owners" $ do
      sqlSet "theme_id"  $ tid
      sqlSet "domain_id" $ did

data DeleteThemeOwnedByDomain = DeleteThemeOwnedByDomain BrandedDomainID ThemeID
instance (MonadDB m,MonadThrow m) => DBUpdate m DeleteThemeOwnedByDomain () where
  update (DeleteThemeOwnedByDomain did tid) = do
    runQuery_ . sqlDelete "themes" $ do
      sqlWhereInSql "id" $ do
        sqlSelect "theme_owners" $ do
          sqlWhereEq "domain_id" $ did
          sqlWhereEq "theme_id" $ tid
          sqlResult "theme_id"
      sqlWhereNotExists $ do -- Never try to delete themes of main domain
        sqlSelect "branded_domains" $ do
          sqlWhereEq "id" $ did
          sqlWhereEq "main_domain" $ True

data DeleteThemeOwnedByCompany = DeleteThemeOwnedByCompany CompanyID ThemeID
instance (MonadDB m,MonadThrow m) => DBUpdate m DeleteThemeOwnedByCompany () where
  update (DeleteThemeOwnedByCompany cid tid) = do
    runQuery_ . sqlDelete "themes" $ do
      sqlWhereInSql "id" $ do
        sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" $ cid
          sqlWhereEq "theme_id" $ tid
          sqlResult "theme_id"

data UnsafeInsertNewThemeWithoutOwner = UnsafeInsertNewThemeWithoutOwner Theme
instance (MonadDB m,MonadThrow m) => DBUpdate m UnsafeInsertNewThemeWithoutOwner Theme where
  update (UnsafeInsertNewThemeWithoutOwner t) = do
    runQuery_ . sqlInsert "themes" $ do
      sqlSet "name" $ themeName t
      sqlSet "logo" $ themeLogo t
      sqlSet "brand_color" $ themeBrandColor t
      sqlSet "brand_text_color" $ themeBrandTextColor t
      sqlSet "action_color" $ themeActionColor t
      sqlSet "action_text_color" $ themeActionTextColor t
      sqlSet "action_secondary_color" $ themeActionSecondaryColor t
      sqlSet "action_secondary_text_color" $ themeActionSecondaryTextColor t
      sqlSet "positive_color" $ themePositiveColor t
      sqlSet "positive_text_color" $ themePositiveTextColor t
      sqlSet "negative_color" $ themeNegativeColor t
      sqlSet "negative_text_color" $ themeNegativeTextColor t
      sqlSet "font" $ themeFont t
      selectThemesSelectors
    fetchOne fetchTheme


selectThemesSelectors :: (SqlResult command) => State command ()
selectThemesSelectors = do
  sqlResult "themes.id"
  sqlResult "themes.name"
  sqlResult "themes.logo"
  sqlResult "themes.brand_color"
  sqlResult "themes.brand_text_color"
  sqlResult "themes.action_color"
  sqlResult "themes.action_text_color"
  sqlResult "themes.action_secondary_color"
  sqlResult "themes.action_secondary_text_color"
  sqlResult "themes.positive_color"
  sqlResult "themes.positive_text_color"
  sqlResult "themes.negative_color"
  sqlResult "themes.negative_text_color"
  sqlResult "themes.font"

selectThemesMD5 :: (SqlResult command) => State command ()
selectThemesMD5 = sqlResult $ "md5(concat(" <> (sqlConcatComma [
     "themes.name"
   , "themes.logo"
   , "themes.brand_color"
   , "themes.brand_text_color"
   , "themes.action_color"
   , "themes.action_text_color"
   , "themes.action_secondary_color"
   , "themes.action_secondary_text_color"
   , "themes.positive_color"
   , "themes.positive_text_color"
   , "themes.negative_color"
   , "themes.negative_text_color"
   , "themes.font"
   ]) <> "))"

fetchTheme :: (ThemeID, String,Binary BS.ByteString, String, String, String, String, String, String, String, String, String, String, String) -> Theme
fetchTheme (tid, name, logo, brand_color, brand_text_color, action_color, action_text_color, action_secondary_color, action_secondary_text_color, positive_color, positive_text_color, negative_color, negative_text_color, font) =
  Theme {
      themeID                       = tid
    , themeName                     = name
    , themeLogo                     = logo
    , themeBrandColor               = brand_color
    , themeBrandTextColor           = brand_text_color
    , themeActionColor              = action_color
    , themeActionTextColor           = action_text_color
    , themeActionSecondaryColor     = action_secondary_color
    , themeActionSecondaryTextColor = action_secondary_text_color
    , themePositiveColor            = positive_color
    , themePositiveTextColor        = positive_text_color
    , themeNegativeColor            = negative_color
    , themeNegativeTextColor        = negative_text_color
    , themeFont                     = font
}
