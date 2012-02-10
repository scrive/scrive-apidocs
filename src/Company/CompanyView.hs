module Company.CompanyView (
    -- pages
    viewCompanySettings
) where

import Company.Model
import Templates.Templates

viewCompanySettings :: TemplatesMonad m => Company -> m String
viewCompanySettings _company = renderTemplateM "viewCompany" ()