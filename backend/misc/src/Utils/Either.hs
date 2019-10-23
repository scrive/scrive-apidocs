module Utils.Either where


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _        = unexpectedError "reading Left for Right"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _         = unexpectedError "reading Right for Left"

toMaybe :: Either a b -> Maybe b
toMaybe (Right a) = Just a
toMaybe _         = Nothing

toEither :: a -> Maybe b -> Either a b
toEither m = maybe (Left m) Right

