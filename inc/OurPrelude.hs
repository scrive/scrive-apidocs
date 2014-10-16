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
  , module Prelude
  ) where

import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (head, last, maximum, minimum, read, tail)
import qualified Prelude as P

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
    fromMaybe (error $ "Reading failed at " ++ $(srcLocation) ++ " (input was '" ++ s ++ "', reads returned " ++ show parsedS ++ ")") $ do
      [(v, "")] <- return parsedS
      return v
  |]

fromJust :: Q Exp
fromJust = [| fromMaybe (error $ "Nothing passed to fromJust at " ++ $(srcLocation)) |]

---- internal stuff below ----

emptyList :: ([a] -> t) -> t -> [a] -> t
emptyList f err v = if null v then err else f v

emptyListError :: String -> Q Exp
emptyListError f =  [| error $ "Empty list passed to " ++ f ++ " at " ++ $(srcLocation) |]

srcLocation :: Q Exp
srcLocation = do
  l <- qLocation
  stringE $ loc_module l ++ ":" ++ show (fst $ loc_start l)
