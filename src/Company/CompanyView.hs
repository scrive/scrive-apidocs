module Company.CompanyView (
    -- pages
    viewCompanySettings
) where

import Templates.Templates
import qualified Templates.Fields as F
import User.Model
import Company.Model

import Data.Maybe

viewCompanySettings :: TemplatesMonad m => (User, Company) -> m String
viewCompanySettings (user, _) = renderTemplate "viewCompany" $ do
  F.value "seessubscriptiondashboard" $ userSeesSubscriptionDashboard user

userSeesSubscriptionDashboard :: User -> Bool
userSeesSubscriptionDashboard user = useriscompanyadmin user || isNothing (usercompany user)
