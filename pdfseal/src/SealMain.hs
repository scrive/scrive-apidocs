module Main where

import Seal
import SealSpec

exrotate :: SealSpec
exrotate = SealSpec 
    { input = "16pages.pdf"
    , output = "16pages_sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , fields = []
    , secretaries = []
    , persons = 
        [ Person 
          { fullname = "Lukas Duczko öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, SkrivaPa öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "some kind of email that is also quite long lukas@duczko.se"
          , company = "Really long company name, CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567"
          , companynumber = "123456-4567"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        ]
    , initials = "öåä, ÖÅÄ"
      , history = [ HistEntry { histdate = "2010-06-01 13:34"
                              , histcomment = "I was here and mucked around with PDFs"
                              }
                  , HistEntry { histdate = "One year later öåä ÖÅÄ"
                              , histcomment = "Still mucking around with PDFs öåä ÖÅÄ"
                              }
                  , HistEntry { histdate = "10 years later öåä ÖÅÄ"
                              , histcomment = "Really soon now öåä ÖÅÄ"
                              }
                  ]
    , staticTexts = sampleSealingTexts                           
    }

gsnorm :: SealSpec
gsnorm = SealSpec 
    { input = "testfiles/gs_norm.pdf"
    , output = "testfiles/gs_norm_sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , fields = []
    , secretaries = []
    , persons = 
        [ Person 
          { fullname = "Lukas Duczko öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, SkrivaPa öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "some kind of email that is also quite long lukas@duczko.se"
          , company = "Really long company name, CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567"
          , companynumber = "123456-4567"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        ]
    , initials = "öåä, ÖÅÄ"
      , history = [ HistEntry { histdate = "2010-06-01 13:34"
                              , histcomment = "I was here and mucked around with PDFs"
                              }
                  , HistEntry { histdate = "One year later öåä ÖÅÄ"
                              , histcomment = "Still mucking around with PDFs öåä ÖÅÄ"
                              }
                  , HistEntry { histdate = "10 years later öåä ÖÅÄ"
                              , histcomment = "Really soon now öåä ÖÅÄ"
                              }
                  ]
    , staticTexts = sampleSealingTexts                           
    }

nda :: SealSpec
nda = SealSpec 
    { input = "nda.pdf"
    , output = "nda_sealed.pdf"
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


twod :: SealSpec
twod = SealSpec 
    { input = "TWOD Custom Compliance Configuration Filled.pdf"
    , output = "TWOD Custom Compliance Configuration Filled and Sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , fields = []
    , secretaries = []
    , persons = 
        [ ]
    , initials = "öåä, ÖÅÄ"
      , history = []
    , staticTexts = sampleSealingTexts         
    }

twodgs :: SealSpec
twodgs = SealSpec 
    { input = "TWOD Custom Compliance Configuration Filled from GS.pdf"
    , output = "TWOD Custom Compliance Configuration Filled from GS and Sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , fields = []
    , secretaries = []
    , persons = 
        [ ]
    , initials = "öåä, ÖÅÄ"
      , history = []
    , staticTexts = sampleSealingTexts         
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
    , staticTexts = sampleSealingTexts           
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
    , staticTexts = sampleSealingTexts           
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
    , staticTexts = sampleSealingTexts       
    } 
-}

worddoc :: SealSpec
worddoc = SealSpec 
    { input = "testfiles/DK Aftale inkassoservice forside 2010.pdf"
    , output = "testfiles/DK Aftale inkassoservice forside 2010 sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , fields = []
    , secretaries = []
    , persons = 
        [ Person 
          { fullname = "Lukas Duczko öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, SkrivaPa öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "some kind of email that is also quite long lukas@duczko.se"
          , company = "Really long company name, CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567"
          , companynumber = "123456-4567"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        ]
    , initials = "öåä, ÖÅÄ"
      , history = [ HistEntry { histdate = "2010-06-01 13:34"
                              , histcomment = "I was here and mucked around with PDFs"
                              }
                  , HistEntry { histdate = "One year later öåä ÖÅÄ"
                              , histcomment = "Still mucking around with PDFs öåä ÖÅÄ"
                              }
                  , HistEntry { histdate = "10 years later öåä ÖÅÄ"
                              , histcomment = "Really soon now öåä ÖÅÄ"
                              }
                  ]
     , staticTexts = sampleSealingTexts                  
    }

formatB5 :: SealSpec
formatB5 = SealSpec 
    { input = "pdfseal/flowchart.pdf"
    , output = "pdfseal/flowchart_sealed.pdf"
    , documentNumber = "0000001234"
    , hostpart = "http://host.skrivapa"
    , fields = []
    , secretaries = []
    , persons = 
        [ Person 
          { fullname = "Lukas Duczko öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, SkrivaPa öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "lukas@duczko.se öåä ÖÅÄ"
          , company = "CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567 öåä ÖÅÄ"
          , companynumber = "123456-4567 öåä ÖÅÄ"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        , Person 
          { fullname = "Żółw Łódź öåä ÖÅÄ"
          , email = "some kind of email that is also quite long lukas@duczko.se"
          , company = "Really long company name, CEO, öåä ÖÅÄ"
          , personalnumber = "123456-4567"
          , companynumber = "123456-4567"
          , fullnameverified = False
          , emailverified = True
          , companyverified = False
          , numberverified = True
          }
        ]
    , initials = "öåä, ÖÅÄ"
      , history = [ HistEntry { histdate = "2010-06-01 13:34"
                              , histcomment = "I was here and mucked around with PDFs"
                              }
                  , HistEntry { histdate = "One year later öåä ÖÅÄ"
                              , histcomment = "Still mucking around with PDFs öåä ÖÅÄ"
                              }
                  , HistEntry { histdate = "10 years later öåä ÖÅÄ"
                              , histcomment = "Really soon now öåä ÖÅÄ"
                              }
                  ]
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

