module TextSearchQuery
  ( TSQuery(..)
  , (<@@>)
  , addTSWildcard
  , mkSimpleQuery
  , mkRawQuery
  , mkSimplePhrase
  , mkSimplePhraseLegacy
  , mkTerm
  ) where

import Data.List (foldl', intersperse)
import Data.List.Extra (nubOrd)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.PostgreSQL.PQTypes.SQL (SQL, mkSQL)
import Prelude
import qualified Data.Text as T

{-
   Partial Haskell-level helpers for text search operators and functions.
   Warning: the naming is crap and should be refactored.
-}

-- | Data type for working with FTS query expressions in PostgreSQL. Having
-- @:&&:@ with a @:<->:@ as parent in the syntax tree is actually sort of
-- nonsensical, but we allow it since PostgreSQL does.
data TSQuery =
     TSQuery :<->: TSQuery
  -- ^ The first term needs to be followed immediately by the second.
  | TSQuery :&&: TSQuery
  -- ^ The two terms need only occur within the `tsvector`.
  | Term Text
  -- ^ A single search term; should contain no spaces. Lowercased before converted.

-- | Construct the SQL string from a @TSQuery@ recursively.
mkRawQuery :: TSQuery -> Text
mkRawQuery (Term t    ) = "'" <> t <> "'" <> "::tsquery"
mkRawQuery (t :&&:  t') = mkRawQuery t <> " && " <> mkRawQuery t'
mkRawQuery (t :<->: t') = mkRawQuery t <> " <-> " <> mkRawQuery t'

-- | Construct the SQL string from a @TSQuery@ recursively and clean by means of
-- PostgreSQL's `to_tsquery`.
mkSimpleQuery :: TSQuery -> Text
mkSimpleQuery (Term t) =
  "to_tsquery('simple', '"
    <> mconcat (intersperse " & " . map addTSWildcard $ cleanUnallowedTSChars t)
    <> "')"
    -- intersperse: when cleaning, e.g. `'asdf:bool'` -> `'asdf bool'` so we
    -- protect against this by inserting `&`; alternative is to analyse and call
    -- recursively.
mkSimpleQuery (t :&&:  t') = mkSimpleQuery t <> " && " <> mkSimpleQuery t'
mkSimpleQuery (t :<->: t') = mkSimpleQuery t <> " <-> " <> mkSimpleQuery t'

-- | Make a `Term`, lowercasing first.
mkTerm :: Text -> TSQuery
mkTerm = Term . T.toLower

-- | Our equivalent of the `@@` FTS matching operator.
infixl 2 <@@>
(<@@>) :: SQL -> TSQuery -> SQL
(<@@>) tsvector qry = tsvector <> " @@ " <> "(" <> mkSQL (mkSimpleQuery qry) <> ")"

-- helpers

cleanUnallowedTSChars :: Text -> [Text]
cleanUnallowedTSChars t = go notAllowed (nubOrd . T.words $ t)
  where
    notAllowed :: [Text]
    notAllowed = ["|", "&", ":", "!", "<->", "<", "*", "'", "(", ")"]

    go :: [Text] -> [Text] -> [Text]
    go []           acc = Prelude.filter (/= "") acc
    go (sym : syms) acc = go syms (nubOrd $ splitAll sym acc)

    splitAll :: Text -> [Text] -> [Text]
    splitAll = concatMap . T.splitOn

addTSWildcard :: Text -> Text
addTSWildcard t = t <> ":*"

-- | PostgreSQL 9.5 does not have phrase search, so we use simple AND:ing while
-- waiting for better times.
mkSimplePhraseLegacy :: Text -> Maybe TSQuery
mkSimplePhraseLegacy txt = case T.words txt of
  []     -> Nothing
  w : ws -> Just $ foldl' (:&&:) (mkTerm w) (map mkTerm ws)

-- | Phrase search; i.e. respect order. Only available in PostgreSQL 9.6 or
-- higher.
mkSimplePhrase :: Text -> Maybe TSQuery
mkSimplePhrase txt = case T.words txt of
  []     -> Nothing
  w : ws -> Just $ foldl' (:<->:) (mkTerm w) (map mkTerm ws)
