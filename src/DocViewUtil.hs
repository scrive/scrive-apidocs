{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocViewUtil (   personname,
                       partyListString,
                       partyListButAuthorString,
                       partyUnsignedListString,
                       partyUnsignedMeAndList,
                       partyUnsignedMeAndListString,
                       makeEditable,
                       withCustom,
                       before,
                       replaceOnEdit,
                       joinWith
           ) where
import AppView
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified HSX.XMLGenerator as HSX
import User
import KontraLink
import Misc
import MinutesTime
import Data.Maybe
import "mtl" Control.Monad.Trans


partyList :: Document -> [SignatoryDetails]
partyList document =
    let author = documentauthordetails document
        signas = map signatorydetails (documentsignatorylinks document)
    in author : signas

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
    swedishListString (map (strong . BS.toString . signatoryname) (partyListButAuthor document))

strong :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
strong x = <strong><% x %></strong>

partyListString :: (XMLGenerator m) => Document -> GenChildList m
partyListString document = 
    swedishListString (map (strong . BS.toString . signatoryname) (partyList document))

partyUnsignedListString :: (XMLGenerator m) => Document -> GenChildList m
partyUnsignedListString document = 
    swedishListString (map (strong . BS.toString . signatoryname) (partyUnsignedList document))

partyUnsignedMeAndListString :: (XMLGenerator m) => MagicHash -> Document -> GenChildList m
partyUnsignedMeAndListString magichash document =
    swedishListString (map (strong . BS.toString . signatoryname) (partyUnsignedMeAndList magichash document))


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
                                  
personname signlink = if (BS.null $ signatoryname $ signatorydetails signlink)
                        then  signatoryemail $ signatorydetails signlink  
                        else  signatoryname $ signatorydetails signlink
              
              
withCustom::(Monad m) => Maybe (BS.ByteString)->(HSPT m XML) ->(HSPT m XML) 
withCustom (Just customMessage) _ = <span> <% (cdata $ BS.toString customMessage ) %> </span>
withCustom Nothing standardMessage =  standardMessage 
                                                        
before::(Monad m) => (HSPT m XML)-> (HSPT m XML)-> (HSPT m XML) 
before header message = <span><span><%header%></span><span><%message%></span><br/></span>                                     
                                           
makeEditable::(Monad m) => String -> (HSPT m XML) ->(HSPT m XML) 
makeEditable name c = <div class="editable" name=name><%c%></div>                      
              
replaceOnEdit this with =  <span> 
                            <span class="replacebynextonedit"> <% this %> </span> 
                            <span style="display:none"> <% with %> </span>  
                           </span>              
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ (joinWith s xs)  