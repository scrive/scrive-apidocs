module Doc.Model.Expressions (
    documentLatestSignTimeExpression
  , documentSignOrderExpression
  ) where

import DB

documentLatestSignTimeExpression :: SQL
documentLatestSignTimeExpression = "(SELECT max(signatory_links.sign_time) FROM signatory_links WHERE signatory_links.document_id = documents.id)"

documentSignOrderExpression :: SQL
documentSignOrderExpression =
  "(COALESCE((SELECT min(signatory_links.sign_order) FROM signatory_links WHERE signatory_links.document_id = documents.id AND signatory_links.is_partner AND signatory_links.sign_time IS NULL), 1))"
