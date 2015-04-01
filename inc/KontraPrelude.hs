module KontraPrelude (
    module Control.Applicative
  , module Control.Monad
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Monoid.Utils
  , module Prelude
  , head
  , last
  , maximum
  , minimum
  , read
  , tail
  , undefined
  , fromJust
  , UnexpectedError(..)
  , unexpectedError
  , unexpectedErrorM
  ) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Catch
import Data.List hiding (head, last, maximum, minimum, tail)
import Data.Maybe hiding (fromJust)
import Data.Monoid
import Data.Monoid.Utils
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (error, head, last, maximum, minimum, read, tail, undefined)
import qualified Prelude as P

data UnexpectedError = UnexpectedError {
  ueMessage  :: !String
, ueModule   :: !String
, ueLine     :: !Int
, uePosition :: !Int
} deriving (Eq, Ord, Typeable)

instance Show UnexpectedError where
  showsPrec _ UnexpectedError{..} = (ueModule ++)
    . (":" ++) . (show ueLine ++)
    . (":" ++) . (show uePosition ++)
    . (": " ++) . (ueMessage ++)

instance Exception UnexpectedError

----------------------------------------

head :: Q Exp
head = [| emptyList P.head $(emptyListError "head") |]

last :: Q Exp
last = [| emptyList P.last $(emptyListError "last") |]

maximum :: Q Exp
maximum = [| emptyList P.maximum $(emptyListError "maximum") |]

minimum :: Q Exp
minimum = [| emptyList P.minimum $(emptyListError "minimum") |]

read :: Q Exp
read = [|
  \s -> let parsedS = reads s in
    fromMaybe ($unexpectedError $ "reading failed (input was '" ++ s ++ "', reads returned '" ++ show parsedS ++ "')") $ do
      [(v, "")] <- return parsedS
      return v
  |]

tail :: Q Exp
tail = [| emptyList P.tail $(emptyListError "tail") |]

undefined :: Q Exp
undefined = [| $unexpectedError ("undefined value"::String) |]

fromJust :: Q Exp
fromJust = [| fromMaybe $ $unexpectedError ("fromJust received Nothing"::String) |]

unexpectedError :: Q Exp
unexpectedError = [|
  \msg -> let (modname, line, position) = $srcLocation
          in throw UnexpectedError {
            ueMessage = msg
          , ueModule = modname
          , ueLine = line
          , uePosition = position
          }
  |]

unexpectedErrorM :: Q Exp
unexpectedErrorM = [|
  \msg -> let (modname, line, position) = $srcLocation
          in throwM UnexpectedError {
            ueMessage = msg
          , ueModule = modname
          , ueLine = line
          , uePosition = position
          }
  |]

---- internal stuff below ----

emptyList :: ([a] -> t) -> t -> [a] -> t
emptyList f err v = if null v then err else f v

emptyListError :: String -> Q Exp
emptyListError fname = [| $unexpectedError $ fname ++ " received an empty list" |]

srcLocation :: Q Exp
srcLocation = do
  Loc{..} <- qLocation
  let (line, position) = loc_start
  runQ [| (loc_module, line, position) |]
