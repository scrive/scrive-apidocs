module Lib.Types.Localization exposing (..)


type alias Localization =
    { cancel : String
    , docsignview : Docsignview
    , eID : EID
    , esigningpoweredbyscrive : String
    , header : Header
    , identify : Identify
    , identifyBankId : String
    , identifyDocument : String
    , identifyFITupasError : IdentifyFITupasError
    , identifyWithPin : IdentifyWithPin
    , idNumber : String
    , ok : String
    , openBankId : String
    , ssnInfoText : String
    , verifyIdentityWithName : String
    , verifyIdentityWithoutName : String
    , yourEmail : String
    , yourIdNumber : String
    , yourMobileNumber : String
    }



-- Broken out so we can use the type aliases as constructors.


type alias Bankid =
    { accessDenied : String
    , invalidParametersCanChange : String
    , invalidParametersCantChange : String
    , rfa1 : String
    , rfa3 : String
    , rfa5 : String
    , rfa6 : String
    , rfa8 : String
    , rfa9 : String
    , rfa12 : String
    , rfa13 : String
    , rfa14 : String
    , rfa16 : String
    , rfa17 : String
    , rfa18 : String
    }


type alias Docsignview =
    { eleg : Eleg
    , networkIssueErrorMessage : String
    }


type alias EID =
    { infoText : EidInfoText
    }


type alias EidInfoText =
    { cpr : String
    }


type alias Eleg =
    { bankid : Bankid
    }


type alias Header =
    { contact : String
    }


type alias Identify =
    { identifyWithIdin : String
    , identifyWithTupas : String
    , identifyWithVerimi : String
    }


type alias IdentifyFITupasError =
    { auth : String
    , blocked : String
    , canceled : String
    , expired : String
    , failed : String
    , revoked : String
    , useragent : String
    }


type alias IdentifyWithPin =
    { enterPin : String
    , invalidPin : String
    , pinInfotext : String
    }
