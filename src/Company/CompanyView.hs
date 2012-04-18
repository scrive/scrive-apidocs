module Company.CompanyView (
    -- pages
    viewCompanySettings
) where

import Templates.Templates

viewCompanySettings :: TemplatesMonad m => m String
viewCompanySettings = renderTemplate_ "viewCompany"
