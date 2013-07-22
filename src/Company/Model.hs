{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Company.Model (
    module Company.CompanyID
  , Company(..)
  , CompanyInfo(..)
  , GetCompanies(..)
  , GetCompany(..)
  , GetCompanyByUserID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
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


fetchCompanies :: MonadDB m => m [Company]
fetchCompanies = kFold decoder []
  where
    decoder acc cid name number address zip' city country
      ip_address_mask_list sms_originator = Company {
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
      } : acc
