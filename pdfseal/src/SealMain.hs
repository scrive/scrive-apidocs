module Main where

import Seal
import SealSpec

simple_text_test :: SealSpec
simple_text_test = SealSpec 
    { input = "test/simple text test.pdf"
    , output = "test/simple text test sealed.pdf"
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
    , persons = 
        [ Person 
          { fullname = "Lukas Duczko"
          , email = "lukas@duczko.se"
          , company = "CEO, SkrivaPå"
          , personalnumber = "123456-4567"
          , companynumber = "123456-4567"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Lukas Duczko"
          , email = "lukas@duczko.se"
          , company = "CEO, SkrivaPå"
          , personalnumber = "123456-4567"
          , companynumber = "123456-4567"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "åöäÖÅÄ Lukas Duczko"
          , email = "lukas@duczko.se"
          , company = "CEO, SkrivaPå"
          , personalnumber = ""
          , companynumber = ""
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        
        ]
    , initials = "LD, LD"
      , history = [ HistEntry { histdate = "2010-09-01 13:34"
                              , histcomment = "I was here and mucked around with PDFs. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good."
                              }
                  , HistEntry { histdate = "One year later"
                              , histcomment = "Still mucking around with PDFs some more. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good."
                              }
                  , HistEntry { histdate = "10 years later"
                              , histcomment = "Really soon now öåä ÖÅÄ. Swedish works. This is actually a very long line of text so we can really see if the line breaking works or maybe not that good."
                              }
                  ]
    -- should be in 4 corners, aligned
    , fields = [ Field {value = "Gracjan Polak", x = 7, y = 7, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 681, y = 7, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 7, y = 1058, page = 1, w = 770, h = 1085}
               , Field {value = "Gracjan Polak", x = 681, y = 1058, page = 1, w = 770, h = 1085}
               , Field {value = "gracjan@mail.com", x = 121, y = 347, page = 1,w = 770, h = 1085}]
    , staticTexts = sampleSealingTexts                        
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
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och styrker att dokument nummer $documenttitle$ har undertecknats"
        , "av parterna och är juridiskt bindande. Kursiverad information är säkert verifierad genom vår tjänst."
        , "Kontrollera dokumentet mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }

main :: IO ()
main = do
    inp <- getContents
    let spec = read inp
    process spec

