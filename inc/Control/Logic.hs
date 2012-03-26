module Control.Logic where

(||^):: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(||^) f g a = f a || g a
infixl 2 ||^

(&&^):: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&^) f g a =  f a && g a
infixl 3 &&^

-- | Simple logical inference operator (arrow)
(=>>) :: Bool -> Bool -> Bool
(=>>) a b = not a || b
infixl 1 =>>

-- | Higher order inference
(=>>^) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(=>>^) a b x = a x =>> b x
infixl 1 =>>^

-- | Conditional choice operator
-- Use it like this: a <| condition |> b  is equivalent to if condition then a else b
-- http://zenzike.com/posts/2011-08-01-the-conditional-choice-operator
(|>) :: Bool -> a -> Maybe a
True  |> _ = Nothing
False |> y = Just y
infixr 0 <|

(<|) :: a -> Maybe a -> a
x <| Nothing = x
_ <| Just y  = y
infixr 0 |>
