
module SealSpec where

data Person = 
    Person { fullname :: String
           , company :: String
           , personalnumber :: String
           , companynumber :: String
           , email :: String
           , fullnameverified :: Bool
           , companyverified :: Bool
           , numberverified :: Bool
           , emailverified :: Bool
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
    , secretaries :: [Person]
    , history :: [HistEntry]
    , initials :: String
    , hostpart :: String
    , fields :: [Field]
    , staticTexts :: SealingTexts
    }
    deriving (Eq,Ord,Show,Read)

data HistEntry = HistEntry
    { histdate :: String
    , histcomment :: String
    }
    deriving (Eq,Ord,Show,Read)


{- |  Static (almoust) text for sealing document. 
      !!!! IMPORTANT Templates for sealing depends on read instance of this class
      If You change this structure sealing WILL fail, unless changes are made to docseal.st
-}
data SealingTexts = SealingTexts
    {   verificationTitle :: String -- Big title at last page
      , docPrefix ::String          -- ex. Doc. nr (last page and all footers)
      , signedText::String          -- ex. Underteknat (all footers)
      , partnerText ::String        -- Header for partner list
      , secretaryText ::String      -- Header for secretary list
      , orgNumberText :: String     -- Info about partner subtext
      , eventsText ::String         -- history table preheader
      , dateText ::String           -- history table date header
      , historyText :: String       -- history table event header
      , verificationFooter ::[String] -- Long text all the end saing that doc was verified
    }
    deriving (Eq,Ord,Show,Read)