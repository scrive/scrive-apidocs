{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}

module Doc.DocViewUtil ( personname,
                         personname',
                         partyList,
                         partyListButAuthor,
                         partySignedList,
                         partyUnsignedMeAndList,
                         partyUnsignedList,  
                         joinWith,
                         emailFromSignLink,
                         renderListTemplate,  
                         buildattach
                       ) where
import Doc.DocState
import Misc
import Templates.Templates 

import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

isSignatory :: SignatoryLink -> Bool
isSignatory sl = SignatoryPartner `elem` signatoryroles sl

hasSigned :: SignatoryLink -> Bool
hasSigned sl = isJust $ maybesigninfo sl

partyList :: Document -> [SignatoryDetails]
partyList document = [signatorydetails sl | sl <- documentsignatorylink document
                                          , isSignatory sl]
  
partyUnsignedList :: Document -> [SignatoryDetails]
partyUnsignedList document = [signatorydetails sl | sl <- documentsignatorylink document
                                                  , isSignatory sl
                                                  , not $ hasSigned sl]

partySignedList :: Document -> [SignatoryDetails]
partySignedList document = [signatorydetails sl | sl <- documentsignatorylink document
                                                  , isSignatory sl
                                                  ,  hasSigned sl]

-- ?? What is this? -EN
partyUnsignedMeAndList :: MagicHash -> Document -> [SignatoryDetails]
partyUnsignedMeAndList magichash document =
    let signalinks = filter isSignatory $ documentsignatorylinks document
        isSignatory person = SignatoryPartner `elem` signatoryroles person
        cond signlink = signatorymagichash signlink /= magichash &&
                        maybesigninfo signlink == Nothing
        unsignalinks = filter cond signalinks
        me = SignatoryDetails { signatoryfstname = BS.fromString "du"
                              , signatorysndname = BS.empty
                              , signatorycompany = BS.empty
                              , signatorypersonalnumber = BS.empty
                              , signatorycompanynumber = BS.empty
                              , signatoryemail = BS.empty
                              , signatorysignorder = SignOrder 1
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
partyListButAuthor document = [signatorydetails sl | sl <- documentsignatorylinks document
                                                   , isSignatory sl
                                                   , not $ siglinkIsAuthor sl]
  
joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ (joinWith s xs)  

{- Either a signatory name or email address. We dont want to show empty strings -}
personname :: SignatoryLink -> BS.ByteString 
personname = personname' . signatorydetails 

{- Same but unwrapped. We need this cause author details are in this format  -}
personname' :: SignatoryDetails -> BS.ByteString 
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

