{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Company.Model (
    module Company.CompanyID
  , Company(..)
  , CompanyInfo(..)
  , CompanyUI(..)
  , GetCompanies(..)
  , GetCompany(..)
  , GetCompanyByUserID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
  , SetCompanyUI(..)
  , GetCompanyUI(..)
  , SetCompanyIPAddressMaskList(..)

  , CompanyFilter(..)
  , CompanyOrderBy(..)
  ) where

import Data.Typeable
import DB
import DB.SQL2
import Company.CompanyID
import Control.Monad.State
import User.UserID
import IPAddress
import OurPrelude

data Company = Company {
    companyid         :: CompanyID
  , companyinfo       :: CompanyInfo
  , companyui         :: CompanyUI
  } deriving (Eq, Ord, Show, Typeable)

data CompanyInfo = CompanyInfo {
    companyname    :: String
  , companynumber  :: String
  , companyaddress :: String
  , companyzip     :: String
  , companycity    :: String
  , companycountry :: String
  , companyipaddressmasklist :: [IPAddressWithMask]
  , companysmsoriginator :: String
  } deriving (Eq, Ord, Show)

data CompanyUI = CompanyUI
  { companyemailbordercolour          :: Maybe String
  , companyemailfont                  :: Maybe String
  , companyemailbuttoncolour          :: Maybe String
  , companyemailemailbackgroundcolour :: Maybe String
  , companyemailbackgroundcolour      :: Maybe String
  , companyemailtextcolour            :: Maybe String
  , companyemaillogo                  :: Maybe Binary
  , companysignviewlogo               :: Maybe Binary
  , companysignviewtextcolour         :: Maybe String
  , companysignviewtextfont           :: Maybe String
  , companysignviewbarscolour         :: Maybe String
  , companysignviewbarstextcolour     :: Maybe String
  , companysignviewbackgroundcolour   :: Maybe String
  , companycustomlogo                 :: Maybe Binary
  , companycustombarscolour           :: Maybe String
  , companycustombarstextcolour       :: Maybe String
  , companycustombarssecondarycolour  :: Maybe String
  , companycustombackgroundcolour     :: Maybe String
} deriving (Eq, Ord, Show, Typeable)

data CompanyFilter
  = CompanyFilterByString String             -- ^ Contains the string anywhere

companyFilterToWhereClause :: (SqlWhere command) => CompanyFilter -> State command ()
companyFilterToWhereClause (CompanyFilterByString text) = do
  -- ALL words from 'text' are in ANY of the fields
  mapM_ (sqlWhere . findWord) (words text)
  where
      findWordInField word field = ("companies." <> field) <+> "ILIKE" <?> sqlwordpat word
      findWordList word = map (findWordInField word) ["name", "number", "address", "zip", "city", "country"]
      findWord word = sqlConcatOR $ findWordList word
      sqlwordpat word = "%" ++ concatMap escape word ++ "%"
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]

data CompanyOrderBy
  = CompanyOrderByName    -- ^ Order by title, alphabetically, case insensitive
  | CompanyOrderByNumber  -- ^ Order by modification time
  | CompanyOrderByAddress -- ^ Order by creation time
  | CompanyOrderByZip     -- ^ Order by status class.
  | CompanyOrderByCity    -- ^ Order by document type.
  | CompanyOrderByCountry -- ^ Order by process
  | CompanyOrderByID      -- ^ Order by process

companyOrderByToOrderBySQL :: CompanyOrderBy -> SQL
companyOrderByToOrderBySQL order =
  case order of
    CompanyOrderByName    -> SQL "companies.name" []
    CompanyOrderByNumber  -> SQL "companies.number" []
    CompanyOrderByAddress -> SQL "companies.address" []
    CompanyOrderByZip     -> SQL "companies.zip" []
    CompanyOrderByCity    -> SQL "companies.city" []
    CompanyOrderByCountry -> SQL "companies.country" []
    CompanyOrderByID      -> SQL "companies.id" []

data GetCompanies = GetCompanies [CompanyFilter] [AscDesc CompanyOrderBy] Integer Integer
instance MonadDB m => DBQuery m GetCompanies [Company] where
  query (GetCompanies filters sorting offset limit) = do
    kRun_ $ sqlSelect "companies" $ do
       sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
       selectCompaniesSelectors
       mapM_ companyFilterToWhereClause filters
       let ascdesc (Asc x) = companyOrderByToOrderBySQL x
           ascdesc (Desc x) = companyOrderByToOrderBySQL x <> SQL " DESC" []
       mapM_ (sqlOrderBy . ascdesc) sorting
       sqlOffset offset
       sqlLimit limit
       sqlOrderBy "companies.id DESC"
    fetchCompanies

data GetCompany = GetCompany CompanyID
instance MonadDB m => DBQuery m GetCompany (Maybe Company) where
  query (GetCompany cid) = do
    kRun_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "id" cid
    fetchCompanies >>= oneObjectReturnedGuard

data GetCompanyByUserID = GetCompanyByUserID UserID
instance MonadDB m => DBQuery m GetCompanyByUserID (Maybe Company) where
  query (GetCompanyByUserID uid) = do
    kRun_ $ sqlSelect "companies" $ do
      sqlJoinOn "users" "users.company_id = companies.id"
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "users.id" uid
    fetchCompanies >>= oneObjectReturnedGuard

data CreateCompany = CreateCompany
instance MonadDB m => DBUpdate m CreateCompany Company where
  update (CreateCompany) = do
    kRun_ $ sqlInsert "companies" $ do
      sqlSetCmd "id" "DEFAULT"
      sqlResult "id"
    (companyidx :: CompanyID) <- kFold (flip (:)) [] >>= exactlyOneObjectReturnedGuard
    kRun_ $ sqlInsert "company_uis" $ do
      sqlSet "company_id" companyidx

    kRun_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "companies.id" companyidx
    fetchCompanies >>= exactlyOneObjectReturnedGuard

data SetCompanyIPAddressMaskList = SetCompanyIPAddressMaskList CompanyID [IPAddress]
instance MonadDB m => DBUpdate m SetCompanyIPAddressMaskList Bool where
  update (SetCompanyIPAddressMaskList cid ads) = do
    kRun01 $ SQL ("UPDATE companies SET"
                  <> "  ip_address_mask = ?"
                  <> "  WHERE id = ?")
             [toSql $ (show ads), toSql cid]


data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance MonadDB m => DBUpdate m SetCompanyInfo Bool where
  update (SetCompanyInfo cid CompanyInfo{..}) =
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "name" companyname
      sqlSet "number" companynumber
      sqlSet "address" companyaddress
      sqlSet "zip" companyzip
      sqlSet "city" companycity
      sqlSet "country" companycountry
      sqlSet "ip_address_mask_list" $ case companyipaddressmasklist of
                                        [] -> Nothing
                                        x -> Just (show x)
      sqlSet "sms_originator" companysmsoriginator
      sqlWhereEq "id" cid

data GetCompanyUI = GetCompanyUI CompanyID
instance MonadDB m => DBQuery m GetCompanyUI CompanyUI where
  query (GetCompanyUI cid) = do
    kRun_ $ sqlSelect "company_uis" $ do
      sqlWhereEq "company_id" cid
      selectCompanyUIsSelectors
    fetchCompanyUIs >>= exactlyOneObjectReturnedGuard

data SetCompanyUI = SetCompanyUI CompanyID CompanyUI
instance MonadDB m => DBUpdate m SetCompanyUI Bool where
  update (SetCompanyUI cid cui) = do
    kRun01 $ sqlUpdate "company_uis" $ do
      sqlSet "email_bordercolour" $ companyemailbordercolour cui
      sqlSet "email_font" $ companyemailfont cui
      sqlSet "email_buttoncolour" $ companyemailbuttoncolour cui
      sqlSet "email_emailbackgroundcolour" $ companyemailemailbackgroundcolour cui
      sqlSet "email_backgroundcolour" $ companyemailbackgroundcolour cui
      sqlSet "email_textcolour" $ companyemailtextcolour cui
      sqlSet "email_logo" $ companyemaillogo cui
      sqlSet "signview_logo" $ companysignviewlogo cui
      sqlSet "signview_textcolour" $ companysignviewtextcolour cui
      sqlSet "signview_textfont" $ companysignviewtextfont cui
      sqlSet "signview_barscolour" $ companysignviewbarscolour cui
      sqlSet "signview_barstextcolour" $ companysignviewbarstextcolour cui
      sqlSet "signview_backgroundcolour" $ companysignviewbackgroundcolour cui
      sqlSet "custom_logo" $ companycustomlogo cui
      sqlSet "custom_barscolour" $ companycustombarscolour cui
      sqlSet "custom_barstextcolour" $ companycustombarstextcolour cui
      sqlSet "custom_barssecondarycolour" $ companycustombarssecondarycolour cui
      sqlSet "custom_backgroundcolour" $ companycustombackgroundcolour cui
      sqlWhereEq "company_id" cid

-- helpers

selectCompaniesSelectors :: (SqlResult command) => State command ()
selectCompaniesSelectors = do
  sqlResult "companies.id"
  sqlResult "companies.name"
  sqlResult "companies.number"
  sqlResult "companies.address"
  sqlResult "companies.zip"
  sqlResult "companies.city"
  sqlResult "companies.country"
  sqlResult "companies.ip_address_mask_list"
  sqlResult "companies.sms_originator"
  selectCompanyUIsSelectors

selectCompanyUIsSelectors :: (SqlResult command) => State command ()
selectCompanyUIsSelectors = do
  sqlResult "company_uis.email_font"
  sqlResult "company_uis.email_bordercolour"
  sqlResult "company_uis.email_buttoncolour"
  sqlResult "company_uis.email_emailbackgroundcolour"
  sqlResult "company_uis.email_backgroundcolour"
  sqlResult "company_uis.email_textcolour"
  sqlResult "company_uis.email_logo"
  sqlResult "company_uis.signview_logo"
  sqlResult "company_uis.signview_textcolour"
  sqlResult "company_uis.signview_textfont"
  sqlResult "company_uis.signview_barscolour"
  sqlResult "company_uis.signview_barstextcolour"
  sqlResult "company_uis.signview_backgroundcolour"
  sqlResult "company_uis.custom_logo"
  sqlResult "company_uis.custom_barscolour"
  sqlResult "company_uis.custom_barstextcolour"
  sqlResult "company_uis.custom_barssecondarycolour"
  sqlResult "company_uis.custom_backgroundcolour"


fetchCompanies :: MonadDB m => m [Company]
fetchCompanies = kFold decoder []
  where
    decoder acc cid name number address zip' city country
      ip_address_mask_list sms_originator email_font
      email_bordercolour email_buttoncolour email_emailbackgroundcolour
      email_backgroundcolour email_textcolour email_logo signview_logo signview_textcolour
      signview_textfont signview_barscolour signview_barstextcolour
      signview_backgroundcolour custom_logo custom_barscolour custom_barstextcolour
      custom_barssecondarycolour custom_backgroundcolour  = Company {
        companyid = cid
      , companyinfo = CompanyInfo {
          companyname = name
        , companynumber = number
        , companyaddress = address
        , companyzip = zip'
        , companycity = city
        , companycountry = country
        , companyipaddressmasklist = maybe [] $(read) ip_address_mask_list
        , companysmsoriginator = sms_originator
        }
      , companyui = CompanyUI {
          companyemailfont = email_font
        , companyemailbordercolour = email_bordercolour
        , companyemailbuttoncolour = email_buttoncolour
        , companyemailemailbackgroundcolour = email_emailbackgroundcolour
        , companyemailbackgroundcolour = email_backgroundcolour
        , companyemailtextcolour = email_textcolour
        , companyemaillogo = email_logo
        , companysignviewlogo = signview_logo
        , companysignviewtextcolour = signview_textcolour
        , companysignviewtextfont = signview_textfont
        , companysignviewbarscolour = signview_barscolour
        , companysignviewbarstextcolour = signview_barstextcolour
        , companysignviewbackgroundcolour = signview_backgroundcolour
        , companycustomlogo  = custom_logo
        , companycustombarscolour = custom_barscolour
        , companycustombarstextcolour = custom_barstextcolour
        , companycustombarssecondarycolour = custom_barssecondarycolour
        , companycustombackgroundcolour = custom_backgroundcolour
        }
      } : acc

fetchCompanyUIs :: MonadDB m => m [CompanyUI]
fetchCompanyUIs = kFold decoder []
  where
    decoder acc email_font
      email_bordercolour email_buttoncolour email_emailbackgroundcolour
      email_backgroundcolour email_textcolour email_logo signview_logo signview_textcolour
      signview_textfont signview_barscolour signview_barstextcolour
      signview_backgroundcolour custom_logo custom_barscolour custom_barstextcolour
      custom_barssecondarycolour custom_backgroundcolour = CompanyUI {
          companyemailfont = email_font
        , companyemailbordercolour = email_bordercolour
        , companyemailbuttoncolour = email_buttoncolour
        , companyemailemailbackgroundcolour = email_emailbackgroundcolour
        , companyemailbackgroundcolour = email_backgroundcolour
        , companyemailtextcolour = email_textcolour
        , companyemaillogo = email_logo
        , companysignviewlogo = signview_logo
        , companysignviewtextcolour = signview_textcolour
        , companysignviewtextfont = signview_textfont
        , companysignviewbarscolour = signview_barscolour
        , companysignviewbarstextcolour = signview_barstextcolour
        , companysignviewbackgroundcolour = signview_backgroundcolour
        , companycustomlogo  = custom_logo
        , companycustombarscolour = custom_barscolour
        , companycustombarstextcolour = custom_barstextcolour
        , companycustombarssecondarycolour = custom_barssecondarycolour
        , companycustombackgroundcolour = custom_backgroundcolour
        } : acc
