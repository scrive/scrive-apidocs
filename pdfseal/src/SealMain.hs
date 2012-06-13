{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Seal
import SealSpec
import System.Directory
import Data.List
import Control.Exception
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Maybe

addBarackObamaField :: IO Field
addBarackObamaField = do
  bin <- BS.readFile "pdfseal/test/barack-obama-signature.jpg"
  let (w1,h1) = (523,247)
  let base64 = Base64.encode bin
  return $ FieldJPG
         { valueBase64 = BS.toString base64
         , internal_image_w = w1
         , internal_image_h = h1
         , image_w = w1
         , image_h = h1
         , x = 7
         , y = 7
         , page = 1
         , w = 770
         , h = 1085
         }

sealspec :: String -> SealSpec
sealspec filename = SealSpec
    { input = filename
    , output = filename ++ "-sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , secretaries = [Person
          { fullname = "Belinda Rossman (secretary)"
          , email = "belinda@rossman.de"
          , company = "Rossman, CO"
          , personalnumber = "435324-222"
          , companynumber = "435324-222"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }]
    , persons = map (\num ->
         Person
          { fullname = "Lukas Duczko " ++ show num
          , email = "lukas@duczko.se"
          , company = "CEO, SkrivaPå"
          , personalnumber = "123456-4567-" ++ show num
          , companynumber = "00006-4567" ++ show num
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }) [1..30::Int]
    , initials = "LD, LD"
      , history = map (\num -> HistEntry { histdate = "2010-09-" ++ show num ++ " 13:34"
                                         , histcomment = "I was here and mucked around with PDFs. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good."
                                         , histaddress = "IP: 123.34.1231.12"
                                         }) [10..99::Int]
    -- should be in 4 corners, aligned
    , fields = [ Field {value = "Gracjan Polak", x = 7, y = 7, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 681, y = 7, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 7, y = 1058, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 681, y = 1058, page = 1, w = 770, h = 1085}
               , Field {value = "gracjan@mail.com", x = 121, y = 347, page = 1,w = 770, h = 1085}]
    , staticTexts = sampleSealingTexts
    , attachments = [ SealAttachment { fileName = "SkrivaPå attachment 1.txt"
                                     , fileBase64Content = "214124124123412341234"
                                     }
                    , SealAttachment { fileName = "SkrivaPå attachment 2.html"
                                     , fileBase64Content = "26345645636534563454"
                                     }
                    , SealAttachment { fileName = "SkrivaPå attachment 2b.pdf"
                                     , fileBase64Content = "2632345234534563454"
                                     }
                    ]
    }


sampleSealingTexts::SealingTexts
sampleSealingTexts = SealingTexts
    {   verificationTitle="Verifikat"
      , docPrefix="Dok.nr."
      , signedText="Undertecknat:"
      , partnerText="Parter"
      , secretaryText="Ej undertecknande part"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=
           "Detta verifikat är utfärdat av Scrive. Kursiverad information är säkert verifierad. Tidsstämpeln säkerställer att dokumentets äkthet går att bevisa matematiskt och oberoende av Scrive. För mer information se den dolda juridiska bilagan. För er bekvämlighet tillhandahåller Scrive även en tjänst för att kontrollera dokumentets äkthet automatiskt på: https://scrive.com/verify/"
    }

processFile :: String -> IO ()
processFile filename = do
  process (sealspec filename)

processWithObama :: SealSpec -> IO ()
processWithObama sealspec = do
  obama <- addBarackObamaField
  let sealspec2 = sealspec { fields = obama : fields sealspec }
  process sealspec2

sealAllInTest :: IO ()
sealAllInTest = do
  contents <- getDirectoryContents "test"
  mapM_ p contents
  where p filename | "sealed.pdf" `isSuffixOf` filename = return ()
                   | ".pdf" `isSuffixOf` filename = do
                       putStrLn $ "Doing " ++ filename
                       processa ("test/" ++ filename)
                   | otherwise = return ()
        processa filename = do
                  result <- try (processFile filename)
                  case result of
                    Left (thing :: SomeException) -> putStrLn (show thing)
                    Right thing -> putStrLn (show thing)

simple_upsales_confirmation :: SealSpec
simple_upsales_confirmation = SealSpec
    { input = "test/upsales-confirmation.pdf"
    , output = "test/upsales-confirmation-sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , secretaries = [Person
          { fullname = "Belinda Rossman (secretary)"
          , email = "belinda@rossman.de"
          , company = "Rossman, CO"
          , personalnumber = "435324-222"
          , companynumber = "435324-222"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }]
    , persons = map (\num ->
         Person
          { fullname = "Lukas Duczko " ++ show num
          , email = "lukas@duczko.se"
          , company = "CEO, SkrivaPå"
          , personalnumber = "123456-4567-" ++ show num
          , companynumber = "00006-4567" ++ show num
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }) [1..30::Int]
    , initials = "LD, LD"
      , history = map (\num -> HistEntry { histdate = "2010-09-" ++ show num ++ " 13:34"
                                         , histcomment = "I was here and mucked around with PDFs. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good."
                                         , histaddress = "IP: 1123.11.131.1231"
                                         }) [10..99::Int]
    -- should be in 4 corners, aligned
    , fields = [ Field {value = "Gracjan Polak", x = 7, y = 7, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 681, y = 7, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 7, y = 1058, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 681, y = 1058, page = 1, w = 770, h = 1085}
               , Field {value = "gracjan@mail.com", x = 121, y = 347, page = 1,w = 770, h = 1085}]
    , staticTexts = sampleSealingTexts
    , attachments = [ SealAttachment { fileName = "SkrivaPa attachment 1.txt"
                                     , fileBase64Content = "214124124123412341234"
                                     }
                    , SealAttachment { fileName = "SkrivaPa attachment 2.html"
                                     , fileBase64Content = "26345645636534563454"
                                     }
                    , SealAttachment { fileName = "SkrivaPa attachment 2b.pdf"
                                     , fileBase64Content = "2632345234534563454"
                                     }
                    ]
    }

main :: IO ()
main = do
    inp <- getContents
    case (listToMaybe . map fst . reads $ inp) of -- Don't try to factor out this to maybeRead, not worth it
        Just spec -> process spec
        Nothing -> case (listToMaybe . map fst . reads $  inp) of
                      Just prespec -> preprocess prespec
                      Nothing -> error "Wrong spec for pdfseal"
