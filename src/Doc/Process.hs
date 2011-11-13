module Doc.Process where

import Doc.DocStateData
import Doc.DocUtils

-- | The building blocks for making a matcher
data DocumentDataMatcher = MatchDocumentStatus DocumentStatus
                         | MatchDocumentType   DocumentType
                         | MatchDocStatusPred  (DocumentStatus -> Bool)
                         | MatchDocTypePred    (DocumentType   -> Bool)
                         | MatchDocAnd DocumentDataMatcher DocumentDataMatcher
                         | MatchDocOr  DocumentDataMatcher DocumentDataMatcher
                         | MatchDocNot DocumentDataMatcher
                         | MatchDocAny
                         | MatchDocNone

-- | The semantics of a DocumentDataMatcher are simple
isDocMatch :: DocumentDataMatcher -> DocumentType -> DocumentStatus -> Bool
isDocMatch (MatchDocumentStatus s1) _ s2 = s1 == s2
isDocMatch (MatchDocumentType   t1) t2 _ = t1 == t2
isDocMatch (MatchDocStatusPred  p ) _ s = p s
isDocMatch (MatchDocTypePred    p ) t _ = p t
isDocMatch (MatchDocAnd a b       ) t s = isDocMatch a t s && isDocMatch b t s
isDocMatch (MatchDocOr a b        ) t s = isDocMatch a t s || isDocMatch b t s
isDocMatch (MatchDocNot a         ) t s = not $ isDocMatch a t s
isDocMatch (MatchDocAny           ) _ _ = True
isDocMatch (MatchDocNone          ) _ _ = False

-- some examples

-- signing is only available in Signable and Pending
signingRestriction :: DocumentDataMatcher
signingRestriction = MatchDocAnd (MatchDocTypePred isSignable) (MatchDocumentStatus Pending)

-- changing the title is only available in Preparation
changeTitleRestriction :: DocumentDataMatcher
changeTitleRestriction = MatchDocumentStatus Preparation

-- we can then define a higher-level abstraction on DBUpdate
class DocumentOperation a b where
  docOp :: a -> DB b -- the Database stuff; you can assume a lock on documents
  restriction :: a -> DocumentDataMatcher -- what are the restrictions on this operation
  restriction _ = MatchDocNone
  getDocumentID :: a -> DocumentID --  the documentid to act on
  
-- and now we make an instance for DBUpdate which performs the check
-- automatically
instance (DocumentOperation a b) => DBUpdate a b where
  dbUpdate a = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
    -- replace next two lines with SQL code to get the status/type
    -- based on getDocumentID a
    -- (docstatus, doctype) <- getStatusAndType $ getDocumentID a
    let docstatus = Preparation
        doctype   = Signable Contract
    if isMatch (restriction a) doctype docstatus 
      then docOp a
      else error "Operation is not allowed."

