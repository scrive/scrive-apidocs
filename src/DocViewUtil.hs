{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module DocViewUtil (   personname,
                       personname',
                       partyList,
                       partyListButAuthor,
                       partyListString,
                       partyListButAuthorString,
                       partyUnsignedListString,
                       partyUnsignedMeAndList,
                       partyUnsignedMeAndListString,
                       partyUnsignedList,  
                       joinWith,
                       addbr,
                       emailFromSignLink,
                       renderListTemplate  
           ) where
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified HSX.XMLGenerator as HSX
import Misc
import Templates.Templates 

partyList :: Document -> [SignatoryDetails]
partyList document = map signatorydetails (documentsignatorylinks document)

partyUnsignedList :: Document -> [SignatoryDetails]
partyUnsignedList document =
    let signalinks = documentsignatorylinks document
        unsignalinks = filter ((== Nothing) . maybesigninfo) signalinks
        signas = map signatorydetails unsignalinks
    in signas

partyUnsignedMeAndList :: MagicHash -> Document -> [SignatoryDetails]
partyUnsignedMeAndList magichash document =
    let signalinks = documentsignatorylinks document
        cond signlink = signatorymagichash signlink /= magichash &&
                        maybesigninfo signlink == Nothing
        unsignalinks = filter cond signalinks
        me = SignatoryDetails { signatoryname = BS.fromString "du"
                              , signatorycompany = BS.empty
                              , signatorynumber = BS.empty
                              , signatoryemail = BS.empty
                              , signatorynameplacements = []
                              , signatorycompanyplacements = []
                              , signatorynumberplacements = []
                              , signatoryemailplacements = []
                              , signatoryotherfields = []
                              }
        signas = map signatorydetails unsignalinks
    in me : signas

partyListButAuthor :: Document -> [SignatoryDetails]
partyListButAuthor document =
    map signatorydetails (documentsignatorylinks document)

partyListButAuthorString :: (XMLGenerator m) => Document -> GenChildList m
partyListButAuthorString document =
    swedishListString (map (strong . BS.toString . personname') (partyListButAuthor document))

partyListString :: (XMLGenerator m) => Document -> GenChildList m
partyListString document = 
    swedishListString (map (strong . BS.toString . personname') (partyList document))

partyUnsignedListString :: (XMLGenerator m) => Document -> GenChildList m
partyUnsignedListString document = 
    swedishListString (map (strong . BS.toString . personname') (partyUnsignedList document))

partyUnsignedMeAndListString :: (XMLGenerator m) => MagicHash -> Document -> GenChildList m
partyUnsignedMeAndListString magichash document =
    swedishListString (map (strong . BS.toString . personname') (partyUnsignedMeAndList magichash document))


swedishListString :: (XMLGenerator m) => [XMLGenT m (HSX.XML m)] -> GenChildList m
swedishListString [] = return []
swedishListString [x] = asChild x
swedishListString [x, y] = do
  list <- sequence [asChild x, asChild " och ", asChild y]
  return (concat list)
swedishListString (x:xs) = do
  list <- sequence [asChild x, asChild ", "]
  list2 <- swedishListString xs
  return (concat list ++ list2)
  
strong :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
strong x = <strong><% x %></strong>
                 
joinWith::[a]->[[a]]->[a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ (joinWith s xs)  

addbr::(HSX.XMLGen m) => BS.ByteString -> [GenChildList m]
addbr text | BS.null text = []
addbr text = [<% text %>, <% <br/> %>]


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
                          then  renderTemplate' templates "morethenonelist" [("list",init list),("last", [last list])]   
                          else  renderTemplate' templates "nomoretheneonelist" [("list",list)]   

