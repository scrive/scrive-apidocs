{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module OurPrelude (
    head
  , last
  , maximum
  , minimum
  , read
  , tail
  , fromJust
  , UnexpectedError(..)
  , unexpectedError
  , unexpectedErrorM
  , module Data.Maybe
  , module Prelude
  ) where

import Control.Exception (throw)
import Control.Monad.Catch
import Data.Maybe hiding (fromJust)
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (head, last, maximum, minimum, read, tail)
import qualified Prelude as P

data UnexpectedError = UnexpectedError {
  ueMessage  :: String
, ueModule   :: String
, ueLine     :: Int
, uePosition :: Int
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

tail :: Q Exp
tail = [| emptyList P.tail $(emptyListError "tail") |]

read :: Q Exp
read = [|
  \s -> let parsedS = reads s in
    fromMaybe ($unexpectedError $ "reading failed (input was '" ++ s ++ "', reads returned '" ++ show parsedS ++ "')") $ do
      [(v, "")] <- return parsedS
      return v
  |]

fromJust :: Q Exp
fromJust = [| fromMaybe $ $unexpectedError ("fromJust received Nothing"::String) |]

unexpectedError :: Q Exp
unexpectedError = [|
  (\msg -> let (modname, line, position) = $srcLocation
          in throw UnexpectedError {
            ueMessage = msg
          , ueModule = modname
          , ueLine = line
          , uePosition = position
          }) :: forall t. String -> t
  |]

unexpectedErrorM :: Q Exp
unexpectedErrorM = [|
  (\msg -> let (modname, line, position) = $srcLocation
          in throwM UnexpectedError {
            ueMessage = msg
          , ueModule = modname
          , ueLine = line
          , uePosition = position
          }) :: forall m t. MonadThrow m => String -> m t
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
