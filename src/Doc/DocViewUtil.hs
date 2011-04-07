{-# OPTIONS_GHC -Wall #-}

module Doc.DocViewUtil (   personname,
                       personname',
                       partyList,
                       partyListButAuthor,
                       partySignedList,
                       partyUnsignedMeAndList,
                       partyUnsignedList,  
                       joinWith,
                       emailFromSignLink,
                       renderListTemplate,  
           ) where
import Doc.DocState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Misc
import Templates.Templates 
import Data.Maybe

partyList :: Document -> [SignatoryDetails]
partyList document = map signatorydetails (documentsignatorylinks document)

partyUnsignedList :: Document -> [SignatoryDetails]
partyUnsignedList document =
    let signalinks = documentsignatorylinks document
        unsignalinks = filter (isNothing . maybesigninfo) signalinks
        signas = map signatorydetails unsignalinks
    in signas

partySignedList :: Document -> [SignatoryDetails]
partySignedList document =
    let signalinks = documentsignatorylinks document
        unsignalinks = filter (isJust . maybesigninfo) signalinks
        signas = map signatorydetails unsignalinks
    in signas

partyUnsignedMeAndList :: MagicHash -> Document -> [SignatoryDetails]
partyUnsignedMeAndList magichash document =
    let signalinks = documentsignatorylinks document
        cond signlink = signatorymagichash signlink /= magichash &&
                        maybesigninfo signlink == Nothing
        unsignalinks = filter cond signalinks
        me = SignatoryDetails { signatoryfstname = BS.fromString "du"
                              , signatorysndname = BS.empty
                              , signatorycompany = BS.empty
                              , signatorypersonalnumber = BS.empty
                              , signatorycompanynumber = BS.empty
                              , signatoryemail = BS.empty
                              , signatoryfstnameplacements = []
                              , signatorysndnameplacements = []
                              , signatorycompanyplacements = []
                              , signatorypersonalnumberplacements = []
                              , signatorycompanynumberplacements = []
                              , signatoryemailplacements = []
                              , signatoryotherfields = []
                              }
        signas = map signatorydetails unsignalinks
    in me : signas

partyListButAuthor :: Document -> [SignatoryDetails]
partyListButAuthor document@Document{ documentauthor=Author authorid } =
    map signatorydetails (filter ((maybe True (/= authorid)) . maybesignatory) (documentsignatorylinks document))

  
joinWith::[a]->[[a]]->[a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ (joinWith s xs)  

{- Either a signatory name or email address. We dont want to show empty strings -}
personname::SignatoryLink -> BS.ByteString 
personname = personname' . signatorydetails 

{- Same but unwrapped. We need this cause author detais are in this format  -}
personname'::SignatoryDetails -> BS.ByteString 
personname' signdetails = if (BS.null $ signatoryname $ signdetails)
                           then  signatoryemail $ signdetails
                           else  signatoryname $ signdetails

{- Function for changing SignatoryLink into our inner email address so u dont have to unwrap every time-}
emailFromSignLink::SignatoryLink->(BS.ByteString,BS.ByteString)
emailFromSignLink sl = (signatoryname $ signatorydetails sl,signatoryemail $ signatorydetails sl) 


renderListTemplate:: KontrakcjaTemplates -> [String] -> IO String
renderListTemplate templates list = if (length list > 1)
                          then  renderTemplate templates "morethenonelist" [("list",init list),("last", [last list])]   
                          else  renderTemplate templates "nomorethanonelist" [("list",list)]   

