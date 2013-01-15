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
         , image_w = fromIntegral w1 / 770
         , image_h = fromIntegral h1 / 1085
         , x = 7 / 770
         , y = 7 / 1085
         , page = 1
         , includeInSummary = True
         , onlyForSummary   = False
         , keyColor = Just (255,255,255)
         }

sealspec :: String -> SealSpec
sealspec filename = SealSpec
    { input = filename
    , output = filename ++ "-sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , secretaries = [Person
          { fullname = "Belinda Rossman ÄÅÖäåö żółw (secretary)"
          , email = "belinda@rossman.de"
          , company = "Rossman, CO"
          , personalnumber = "435324-222"
          , companynumber = "435324-222"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          , fields = []
          }]
    , persons = map (\num ->
         Person
          { fullname = "Lukas Duczko ÄÅÖäåö żółw ŻÓŁW " ++ show num
          , email = "lukas@duczko.se ÄÅÖäåö żółw ŻÓŁW"
          , company = "CEO, SkrivaPå, ÄÅÖäåö żółw ŻÓŁW"
          , personalnumber = "żółw ŻÓŁW ÄÅÖäåö 123456-4567-" ++ show num
          , companynumber = "żółw ŻÓŁW ÄÅÖäåö 00006-4567" ++ show num
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          -- should be in 4 corners, aligned
          , fields = [ Field { value = "Gracjan Polak ÄÅÖäåö"
                             , x = 7/770,   y = 7/1085
                             , page = 1
                             , includeInSummary = True
                             , fontSize = 20/770
                             , greyed = False 
                             }
                     , Field { value = "Gracjan Polak"
                             , x = 681/770, y = 7/1085
                             , page = 1
                             , includeInSummary = False
                             , fontSize = 10/770
                             , greyed = False 
                             }
                     , Field { value = "Gracjan Polak"
                             , x = 7/770,   y = 1058/1085
                             , page = 1
                             , includeInSummary = False
                             , fontSize = 10/770
                             , greyed = False 
                             }
                     , Field { value = "Gracjan Polak żółw ŻÓŁW"
                             , x = 681/770, y = 1058/1085
                             , page = 1
                             , includeInSummary = False
                             , fontSize = 10/770
                             , greyed = False 
                             }
                     , Field { value = "gracjan@mail.com ÄÅÖäåö żółw ŻÓŁW"
                             , x = 121/770, y = 347/1085
                             , page = 1
                             , includeInSummary = False
                             , fontSize = 10/770
                             , greyed = False 
                             }
                     ]
          }) [1..10::Int]
    , initials = "LD, LD, żółw ŻÓŁW"
    , history = map (\num -> HistEntry { histdate = "2010-09-" ++ show num ++ " 13:34"
                                         , histcomment = "I was here and mucked around with PDFs. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good. żółw ŻÓŁW, ÄÅÖäåö"
                                         , histaddress = "IP: 123.34.1231.12"
                                         }) [10..99::Int]
    , staticTexts = sampleSealingTexts
    , attachments = [ SealAttachment { fileName = "SkrivaPå attachment 1 żółw ŻÓŁW ÄÅÖäåö.txt"
                                     , fileBase64Content = "214124124123412341234"
                                     }
                    , SealAttachment { fileName = "SkrivaPå attachment 2 żółw ŻÓŁW ÄÅÖäåö.html"
                                     , fileBase64Content = "26345645636534563454"
                                     }
                    , SealAttachment { fileName = "SkrivaPå attachment 2b żółw ŻÓŁW ÄÅÖäåö.pdf"
                                     , fileBase64Content = "2632345234534563454"
                                     }
                    ]
    , filesList = [ FileDesc
                    { fileTitle      = "żółw ŻÓŁW ÄÅÖäåö"
                    , fileRole       = "żółw ŻÓŁW ÄÅÖäåö"
                    , filePagesText  = "żółw ŻÓŁW ÄÅÖäåö"
                    , fileAttachedBy = "żółw ŻÓŁW ÄÅÖäåö"
                    }]
    }


sampleSealingTexts::SealingTexts
sampleSealingTexts = SealingTexts
    {   verificationTitle="Verifikat żółw ŻÓŁW ÄÅÖäåö"
      , docPrefix="Dok.nr. żółw ŻÓŁW ÄÅÖäåö"
      , signedText="Undertecknat: żółw ŻÓŁW ÄÅÖäåö"
      , partnerText="Parter żółw ŻÓŁW ÄÅÖäåö"
      , secretaryText="Ej undertecknande part żółw ŻÓŁW ÄÅÖäåö"
      , orgNumberText="Org.nr. żółw ŻÓŁW ÄÅÖäåö"
      , eventsText="Registrerade händelser żółw ŻÓŁW ÄÅÖäåö"
      , dateText="Datum żółw ŻÓŁW ÄÅÖäåö"
      , historyText="Händelser żółw ŻÓŁW ÄÅÖäåö"
      , verificationFooter=
           "Detta verifikat är utfärdat av Scrive. Kursiverad information är säkert verifierad. Tidsstämpeln säkerställer att dokumentets äkthet går att bevisa matematiskt och oberoende av Scrive. För mer information se den dolda juridiska bilagan. För er bekvämlighet tillhandahåller Scrive även en tjänst för att kontrollera dokumentets äkthet automatiskt på: https://scrive.com/verify/ żółw ŻÓŁW"
      , documentText = "Dokument żółw ŻÓŁW ÄÅÖäåö"
      , personalNumberText = "ID-nr. żółw ŻÓŁW ÄÅÖäåö"
    }

processFile :: String -> IO ()
processFile filename = do
  let ss1 = sealspec filename
  obama <- addBarackObamaField
  let addObama person = person { fields = obama : fields person }
  let ss = ss1 { persons = addObama (head (persons ss1)) : tail (persons ss1) }
  let pss = PreSealSpec { pssInput = input ss
                        , pssOutput = input ss ++ "-presealed.pdf"
                        , pssFields = concatMap fields (persons ss ++ secretaries ss)
                        }
  preprocess pss
  process ss

sealAllInTest :: IO ()
sealAllInTest = do
  let dir = "test/pdfs"
  contents <- getDirectoryContents dir
  mapM_ (p dir) contents
  where p dir filename | "sealed.pdf" `isSuffixOf` filename = return ()
                       | ".pdf" `isSuffixOf` filename = do
                              let fullname = dir ++ "/" ++ filename
                              putStrLn $ "Doing " ++ fullname
                              processa fullname
                       | otherwise = return ()
        processa filename = do
                  result <- try $ do
                    processFile filename
                  case result of
                    Left (thing :: SomeException) -> putStrLn (show thing)
                    Right _ -> return ()

sealSimple :: IO ()
sealSimple = do
  let dir = "test/pdfs"
  p dir "simple.pdf"
  where p dir filename | "sealed.pdf" `isSuffixOf` filename = return ()
                       | ".pdf" `isSuffixOf` filename = do
                              let fullname = dir ++ "/" ++ filename
                              putStrLn $ "Doing " ++ fullname
                              processa fullname
                       | otherwise = return ()
        processa filename = do
                  result <- try $ do
                    processFile filename
                  case result of
                    Left (thing :: SomeException) -> putStrLn (show thing)
                    Right _ -> return ()


main :: IO ()
main = do
    inp <- getContents
    case (listToMaybe . map fst . reads $ inp) of -- Don't try to factor out this to maybeRead, not worth it
        Just spec -> process spec
        Nothing -> case (listToMaybe . map fst . reads $  inp) of
                      Just prespec -> preprocess prespec
                      Nothing -> error "Wrong spec for pdfseal"
