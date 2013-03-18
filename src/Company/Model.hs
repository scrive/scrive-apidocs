{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Company.Model (
    module Company.CompanyID
  , ExternalCompanyID(..)
  , Company(..)
  , CompanyInfo(..)
  , CompanyUI(..)
  , GetCompanies(..)
  , GetCompany(..)
  , GetCompanyByUserID(..)
  , GetCompanyByExternalID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
  , UpdateCompanyUI(..)
  , GetOrCreateCompanyWithExternalID(..)
  , GetCompanyByEmailDomain(..)
  , SetCompanyEmailDomain(..)
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

-- to be removed, only upsales used that
newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: String }
  deriving (Eq, Ord, Show)
$(newtypeDeriveConvertible ''ExternalCompanyID)

data Company = Company {
    companyid         :: CompanyID
  , companyexternalid :: Maybe ExternalCompanyID
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
  , companyemaildomain :: Maybe String
  , companyipaddressmasklist :: [IPAddressWithMask]
  } deriving (Eq, Ord, Show)

data CompanyUI = CompanyUI {
    companyemailbordercolour :: Maybe String
  , companyemailheaderfont :: Maybe String
  , companyemailfont :: Maybe String
  , companyemailbuttoncolour :: Maybe String
  , companyemailemailbackgroundcolour :: Maybe String
  , companyemailbackgroundcolour :: Maybe String
  , companyemailtextcolour :: Maybe String
  , companyemaillogo :: Maybe Binary
  , companysignviewlogo :: Maybe Binary
  , companysignviewtextcolour :: Maybe String
  , companysignviewtextfont :: Maybe String
  , companysignviewbarscolour :: Maybe String
  , companysignviewbarstextcolour :: Maybe String
  , companysignviewbackgroundcolour :: Maybe String
} deriving (Eq, Ord, Show)

data CompanyFilter
  = CompanyFilterByString String             -- ^ Contains the string anywhere

companyFilterToWhereClause :: (SqlWhere command) => CompanyFilter -> State command ()
companyFilterToWhereClause (CompanyFilterByString text) = do
  -- ALL words from 'text' are in ANY of the fields
  mapM_ (sqlWhere . findWord) (words text)
  where
      findWordInField word field = ("companies." <> field) <+> "ILIKE" <?> sqlwordpat word
      findWordList word = map (findWordInField word) ["name", "number", "address", "zip", "city", "country", "email_domain"]
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
      selectCompaniesSelectors
      sqlWhereEq "id" cid
    fetchCompanies >>= oneObjectReturnedGuard

data GetCompanyByExternalID = GetCompanyByExternalID ExternalCompanyID
instance MonadDB m => DBQuery m GetCompanyByExternalID (Maybe Company) where
  query (GetCompanyByExternalID ecid) = do
    kRun_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhereEq "external_id" ecid
    fetchCompanies >>= oneObjectReturnedGuard

data GetCompanyByUserID = GetCompanyByUserID UserID
instance MonadDB m => DBQuery m GetCompanyByUserID (Maybe Company) where
  query (GetCompanyByUserID uid) = do
    kRun_ $ sqlSelect "companies" $ do
      sqlJoinOn "users" "users.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "users.id" uid
    fetchCompanies >>= oneObjectReturnedGuard

data CreateCompany = CreateCompany (Maybe ExternalCompanyID)
instance MonadDB m => DBUpdate m CreateCompany Company where
  update (CreateCompany mecid) = do
    kRun_ $ sqlInsert "companies" $ do
      sqlSet "external_id" mecid
      selectCompaniesSelectors
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
      sqlSet "email_domain" companyemaildomain
      sqlSet "ip_address_mask_list" $ case companyipaddressmasklist of
                                        [] -> Nothing
                                        x -> Just (show x)
      sqlWhereEq "id" cid

data UpdateCompanyUI = UpdateCompanyUI CompanyID CompanyUI
instance MonadDB m => DBUpdate m UpdateCompanyUI Bool where
  update (UpdateCompanyUI cid cui) = do
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "email_bordercolour" $ companyemailbordercolour cui
      sqlSet "email_headerfont" $ companyemailheaderfont cui
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
      sqlWhereEq "id" cid

data GetOrCreateCompanyWithExternalID = GetOrCreateCompanyWithExternalID ExternalCompanyID
instance MonadDB m => DBUpdate m GetOrCreateCompanyWithExternalID Company where
  update (GetOrCreateCompanyWithExternalID ecid) = do
    mc <- query $ GetCompanyByExternalID ecid
    case mc of
      Just c  -> return c
      Nothing -> update $ CreateCompany $ Just ecid

data SetCompanyEmailDomain = SetCompanyEmailDomain CompanyID (Maybe String)
instance MonadDB m => DBUpdate m SetCompanyEmailDomain Bool where
  update (SetCompanyEmailDomain cid mdomain) = do
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "email_domain" mdomain
      sqlWhereEq "id" cid
      sqlWhere $ SQL "NOT EXISTS (SELECT 1 FROM companies WHERE email_domain = ?)" [toSql mdomain]

data GetCompanyByEmailDomain = GetCompanyByEmailDomain String
instance MonadDB m => DBQuery m GetCompanyByEmailDomain (Maybe Company) where
  query (GetCompanyByEmailDomain domain) = do
    kRun_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhereEq "email_domain" domain
    fetchCompanies >>= oneObjectReturnedGuard

-- helpers

selectCompaniesSelectors :: (SqlResult command) => State command ()
selectCompaniesSelectors = do
  sqlResult "companies.id"
  sqlResult "companies.external_id"
  sqlResult "companies.name"
  sqlResult "companies.number"
  sqlResult "companies.address"
  sqlResult "companies.zip"
  sqlResult "companies.city"
  sqlResult "companies.country"
  sqlResult "companies.email_domain"
  sqlResult "companies.ip_address_mask_list"
  sqlResult "companies.email_headerfont"
  sqlResult "companies.email_font"
  sqlResult "companies.email_bordercolour"
  sqlResult "companies.email_buttoncolour"
  sqlResult "companies.email_emailbackgroundcolour"
  sqlResult "companies.email_backgroundcolour"
  sqlResult "companies.email_textcolour"
  sqlResult "companies.email_logo"
  sqlResult "companies.signview_logo"
  sqlResult "companies.signview_textcolour"
  sqlResult "companies.signview_textfont"
  sqlResult "companies.signview_barscolour"
  sqlResult "companies.signview_barstextcolour"
  sqlResult "companies.signview_headertextcolour"
  sqlResult "companies.signview_headertextfont"
  sqlResult "companies.signview_headerbackgroundcolour"
  sqlResult "companies.signview_footerbackgroundcolour"
  sqlResult "companies.signview_backgroundcolour"


fetchCompanies :: MonadDB m => DBEnv m [Company]
fetchCompanies = kFold decoder []
  where
    decoder acc cid eid name number address zip' city country
      email_domain ip_address_mask_list email_headerfont email_font
      email_bordercolour email_buttoncolour email_emailbackgroundcolour
      email_backgroundcolour email_textcolour email_logo signview_logo signview_textcolour
      signview_textfont signview_barscolour signview_barstextcolour
      signview_backgroundcolour = Company {
        companyid = cid
      , companyexternalid = eid
      , companyinfo = CompanyInfo {
          companyname = name
        , companynumber = number
        , companyaddress = address
        , companyzip = zip'
        , companycity = city
        , companycountry = country
        , companyemaildomain = email_domain
        , companyipaddressmasklist = maybe [] $(read) ip_address_mask_list
        }
      , companyui = CompanyUI {
          companyemailheaderfont = email_headerfont
        , companyemailfont = email_font
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
        }
      } : acc
