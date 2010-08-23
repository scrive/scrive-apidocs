{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where

import Seal
import PdfModel
import System.Environment
import Data.Maybe
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.State.Strict
import qualified Data.Map as Map

exrotate = SealSpec 
    { input = "16pages.pdf"
    , output = "16pages_sealed.pdf"
    , documentNumber = "0000001234"
    , persons = 
        [ Person 
          { fullname = "Lukas Duczko öåä ÖÅÄ"
          , email = "lukas@duczko.se"
          , company = "CEO, SkrivaPa"
          , number = "123456-4567"
          }
        ]
    , initials = "GP, LD"
      , history = [ HistEntry { histdate = "2010-06-01 13:34"
                              , histcomment = "I was here and mucked around with PDFs"
                              }
                  , HistEntry { histdate = "Year later"
                              , histcomment = "Still mucking around with PDFs"
                              }
                  , HistEntry { histdate = "10 years later"
                              , histcomment = "Really soon now"
                              }
                  ]
    }


{-

ex1 = SealSpec 
    { sealInput = "1.pdf"
    , sealOutput = "1_sealed.pdf"
    , sealDocumentNumber = 1234
    , sealPersons = 
        [ SealPerson "Lukas Duczko öåä ÖÅÄ" "CEO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 2 Wolak" "CTO skrivaPa, Stockholm, 2010-05-31"
        ]
    }

ex_all = [ SealSpec 
    { sealInput = show i ++ ".pdf"
    , sealOutput = show i ++ "_sealed.pdf"
    , sealDocumentNumber = 1234567
    , sealPersons = 
        [ SealPerson "Lukas Duczko" "CEO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 2 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 3 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 4 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 5 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 6 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 7 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        ]
    } | i <- [1..10]]

ex_10_1 = SealSpec 
    { sealInput = "10_1.pdf"
    , sealOutput = "10_1_sealed.pdf"
    , sealDocumentNumber = 1234567
    , sealPersons = 
        [ SealPerson "Lukas Duczko" "CEO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 2 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 3 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 4 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 5 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 6 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        , SealPerson "Gracjan 7 Polak" "CTO skrivaPa, Stockholm, 2010-05-31"
        ]
    } 
-}

main = do
    inp <- getContents
    let spec = read inp
    process spec

