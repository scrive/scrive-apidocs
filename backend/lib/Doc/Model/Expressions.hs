module Doc.Model.Expressions (
    documentLatestSignTimeExpression
  , documentSignOrderExpression
  ) where

import DB
import Doc.Types.SignatoryLink

documentLatestSignTimeExpression :: SQL
documentLatestSignTimeExpression =
  parenthesize
    $   "SELECT max(signatory_links.sign_time)"
    <+> "FROM signatory_links"
    <+> "WHERE signatory_links.document_id = documents.id"

documentSignOrderExpression :: SQL
documentSignOrderExpression =
  parenthesize
    $   "COALESCE((SELECT min(signatory_links.sign_order)"
    <+> "FROM signatory_links"
    <+> "WHERE signatory_links.document_id = documents.id"
    <+> "AND"
    <+> parenthesize
          (   "signatory_links.signatory_role ="
          <?> SignatoryRoleSigningParty
          <+> "OR"
          <+> "signatory_links.signatory_role ="
          <?> SignatoryRoleApprover
          )
    <+> "AND signatory_links.sign_time IS NULL), 1)"
