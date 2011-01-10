
module SealSpec where

data Person = 
    Person { fullname :: String
           , company :: String
           , number :: String
           , email :: String
           }
    deriving (Eq,Ord,Show,Read)

data Field =
    Field { value :: String
          , x :: Int
          , y :: Int
          , page :: Int
          , w :: Int
          , h :: Int 
          }
    deriving (Eq, Ord, Show, Read)

data SealSpec = SealSpec 
    { input :: String
    , output :: String
    , documentNumber :: String
    , persons :: [Person]
    , history :: [HistEntry]
    , initials :: String
    , hostpart :: String
    , fields :: [Field]
    }
    deriving (Eq,Ord,Show,Read)

data HistEntry = HistEntry
    { histdate :: String
    , histcomment :: String
    }
    deriving (Eq,Ord,Show,Read)

