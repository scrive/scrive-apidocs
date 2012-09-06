module Company.Model (
    module Company.CompanyID
  , ExternalCompanyID(..)
  , Company(..)
  , CompanyInfo(..)
  , CompanyUI(..)
  , GetCompanies(..)
  , GetCompany(..)
  , GetCompanyByExternalID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
  , UpdateCompanyUI(..)
  , GetOrCreateCompanyWithExternalID(..)
  , GetCompanyByEmailDomain(..)
  , SetCompanyEmailDomain(..)

  , CompanyDomain(..)
  , CompanyFilter(..)
  , CompanyOrderBy(..)
  ) where

import Data.Typeable

import DB
import DB.SQL2
import API.Service.Model
import Company.CompanyID
import Control.Monad.State
import Data.Monoid
import Utils.Monoid
import Data.List

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: String }
  deriving (Eq, Ord, Show)
$(newtypeDeriveConvertible ''ExternalCompanyID)

data Company = Company {
    companyid         :: CompanyID
  , companyexternalid :: Maybe ExternalCompanyID
  , companyservice    :: Maybe ServiceID
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
  } deriving (Eq, Ord, Show)

data CompanyUI = CompanyUI {
    companybarsbackground    :: Maybe String
  , companybarstextcolour    :: Maybe String
  , companylogo              :: Maybe Binary -- File with the logo
} deriving (Eq, Ord, Show)

data CompanyDomain
  = CompaniesOfWholeUniverse                     -- ^ All companies in the system. Only for admin view.
  | CompaniesOfService (Maybe ServiceID)

companyDomainToSQL :: CompanyDomain -> SQL
companyDomainToSQL CompaniesOfWholeUniverse =
  SQL"TRUE" []
companyDomainToSQL (CompaniesOfService Nothing) =
  SQL "companies.service_id IS NULL" []
companyDomainToSQL (CompaniesOfService mservice) =
  SQL "companies.service_id = ?" [toSql mservice]

data CompanyFilter
  = CompanyFilterByString String             -- ^ Contains the string anywhere

companyFilterToWhereClause :: (SqlWhere command) => CompanyFilter -> State command ()
companyFilterToWhereClause (CompanyFilterByString text) = do
  -- ALL words from 'text' are in ANY of the fields
  mapM_ (sqlWhere . findWord) (words text)
  where
      findWordInField word field =
        SQL ("companies." ++ field ++ " ILIKE ?") [sqlwordpat word]
      findWordList word = map (findWordInField word) ["name", "number", "address", "zip", "city", "country", "email_domain"]
      findWord word = mconcat $ intersperse (SQL " OR " []) $ findWordList word
      sqlwordpat word = toSql $ "%" ++ concatMap escape word ++ "%"
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

data GetCompanies = GetCompanies [CompanyDomain] [CompanyFilter] [AscDesc CompanyOrderBy] Integer Integer
instance MonadDB m => DBQuery m GetCompanies [Company] where
  query (GetCompanies domains filters sorting offset limit) = do
    kRun_ $ sqlSelect "companies" $ do
       selectCompaniesSelectors
       let orx [] = SQL "FALSE" []
           orx [thing] = thing
           orx xs = mconcat $ [SQL "(" []] ++ intersperse (SQL " OR " []) xs ++ [SQL ")" []]
       sqlWhere (orx $ map companyDomainToSQL domains)
       mapM_ companyFilterToWhereClause filters
       let ascdesc (Asc x) = companyOrderByToOrderBySQL x
           ascdesc (Desc x) = companyOrderByToOrderBySQL x <++> SQL " DESC" []
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

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance MonadDB m => DBQuery m GetCompanyByExternalID (Maybe Company) where
  query (GetCompanyByExternalID msid ecid) = do
    kRun_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhere $ SQL "service_id IS NOT DISTINCT FROM ?" [toSql msid]
      sqlWhereEq "external_id" ecid
    fetchCompanies >>= oneObjectReturnedGuard

data CreateCompany = CreateCompany (Maybe ServiceID) (Maybe ExternalCompanyID)
instance MonadDB m => DBUpdate m CreateCompany Company where
  update (CreateCompany msid mecid) = do
    kRun_ $ sqlInsert "companies" $ do
      sqlSet "external_id" mecid
      sqlSet "service_id" msid
      selectCompaniesSelectors
    fetchCompanies >>= exactlyOneObjectReturnedGuard

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
      sqlWhereEq "id" cid

data UpdateCompanyUI = UpdateCompanyUI CompanyID CompanyUI
instance MonadDB m => DBUpdate m UpdateCompanyUI Bool where
  update (UpdateCompanyUI cid cui) = do
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "bars_background" $ companybarsbackground cui
      sqlSet "bars_textcolour" $ companybarstextcolour cui
      sqlSet "logo" $ companylogo cui
      sqlWhereEq "id" cid

data GetOrCreateCompanyWithExternalID = GetOrCreateCompanyWithExternalID (Maybe ServiceID) ExternalCompanyID
instance MonadDB m => DBUpdate m GetOrCreateCompanyWithExternalID Company where
  update (GetOrCreateCompanyWithExternalID msid ecid) = do
    mc <- query $ GetCompanyByExternalID msid ecid
    case mc of
      Just c  -> return c
      Nothing -> update $ CreateCompany msid $ Just ecid

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
  sqlResult "id"
  sqlResult "external_id"
  sqlResult "service_id"
  sqlResult "name"
  sqlResult "number"
  sqlResult "address"
  sqlResult "zip"
  sqlResult "city"
  sqlResult "country"
  sqlResult "bars_background"
  sqlResult "bars_textcolour"
  sqlResult "logo"
  sqlResult "email_domain"


fetchCompanies :: MonadDB m => DBEnv m [Company]
fetchCompanies = foldDB decoder []
  where
    decoder acc cid eid sid name number address zip' city country
      bars_background bars_textcolour logo email_domain = Company {
        companyid = cid
      , companyexternalid = eid
      , companyservice = sid
      , companyinfo = CompanyInfo {
          companyname = name
        , companynumber = number
        , companyaddress = address
        , companyzip = zip'
        , companycity = city
        , companycountry = country
        , companyemaildomain = email_domain
        }
      , companyui = CompanyUI {
          companybarsbackground = bars_background
        , companybarstextcolour = bars_textcolour
        , companylogo = logo
        }
      } : acc
