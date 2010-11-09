{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocViewMail ( remindMail,
                     remindMailContent,
                     mailDocumentRejectedForAuthor,
                     rejectMailContent,
                     mailDocumentClosedForAuthor, 
                     mailDocumentClosedForSignatories,
                     mailInvitationToSign,
                     mailInvitationToSignContent
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
import SendMail(Mail,emptyMail,content,title,attachments)
import DocViewUtil
import "mtl" Control.Monad.Trans

remindMail:: Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMail cm c d s = case s of 
                       SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSigned cm c d s
                       _ -> remindMailSigned cm c d s
                       
remindMailContent :: (Monad m) => Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink ->(HSPT m XML) 
remindMailContent cm c d s = case s of 
                               SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s
                               _ -> remindMailSignedContent False cm c d s
                       
remindMailNotSigned::Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMailNotSigned customMessage ctx@Context{ctxmaybeuser = Just user, ctxhostpart}
           document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
           signlink = 
    let link = ctxhostpart ++ show (LinkSignDoc document signlink)
        content  = remindMailNotSignedContent True customMessage ctx  document signlink                 
        attachmentcontent = filepdf $ head $ documentfiles document          
        title =  BS.concat [BS.fromString "Hej ",personname signlink]  
      in 
       do
        content <- htmlHeadBodyWrapIO documenttitle content
        return $ emptyMail {title = title, content = content}

        
        
remindMailSigned::Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned customMessage ctx@Context{ctxmaybeuser = Just user, ctxhostpart}
                 document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
                 signlink = 
                     let 
                         title =  BS.concat [BS.fromString "Hej ",personname signlink]              
                         attachmentcontent = filepdf $ head $ documentfiles document   
                         content = remindMailSignedContent False customMessage ctx  document signlink                             
                     in do
                       content <- htmlHeadBodyWrapIO documenttitle content
                       return $ emptyMail {title = title, content = content, attachments = [(documenttitle,attachmentcontent)]}

remindMailNotSignedContent::(Monad m) => Bool ->  (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> (HSPT m XML)                                       
remindMailNotSignedContent forMail customMessage ctx  document signlink =  
               let link =  (ctxhostpart ctx) ++ show (LinkSignDoc document signlink)
                   link' =  if (forMail)
                             then <a href=link><% link %></a>
                             else <a href="#"> https://skrivapa.se/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/</a>
                   creatorname = signatoryname $ documentauthordetails document         
                   content = <span>
                              <p><i>Översiktlig information</i><br/>
                              Parter: <% partyListString document %><br/>
                              Har undertecknat: <strong><% signatoryname $ documentauthordetails document %></strong><br/>
                              <% case (documenttimeouttime document) of 
                                 Just time -> <span>Undertecknas senast: <strong><% show time %></strong>.</span>
                                 Nothing -> <span/>
                              %> 
                             </p>
                             <p><i>Så här går du vidare</i><br/>
                             1. Klicka på länken<br/>
                              <% link' %><br/>
                             2. Granska online<br/>
                             3. Underteckna<br/>
                             4. Färdig<br/>
                             </p>
                             </span>
                   standardFooter =  if (forMail)
                                     then poweredBySkrivaPaPara (ctxhostpart ctx)
                                     else replaceOnEdit (poweredBySkrivaPaPara (ctxhostpart ctx)) customFooter 
                   customFooter =   <p>
                                    <small>
                                    Hälsningar
                                    <br/>
                                    <% creatorname %>
                                    </small> 
                                    </p>            
                   footer = if (isNothing customMessage) then standardFooter else customFooter                
                   header   = withCustom  customMessage (remindMailNotSignedStandardHeader ctx document signlink)
                   in (makeEditable "customtext" header) `before` content `before` footer 

remindMailSignedContent ::(Monad m) => Bool -> (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> (HSPT m XML)                  
remindMailSignedContent forMail customMessage ctx  document signlink        
                         = makeEditable "customtext" $
                            withCustom 
                                customMessage $
                                (remindMailSignedStandardHeader ctx document signlink) `before` (poweredBySkrivaPaPara (ctxhostpart ctx))                     
                                      
                                      
                                      
remindMailSignedStandardHeader::(Monad m) => Context -> Document -> SignatoryLink -> (HSPT m XML) 
remindMailSignedStandardHeader ctx@Context{ctxmaybeuser = Just user} document signlink =        
                                               <span>
                                                 <p>Hej <%(personname signlink)%><%"\n"%><%"\n"%></p>
                                                 <p><%(userfullname user)%> har begärt att vi ska skicka dokumentet <% documenttitle document %> till dig som du har undertecknat via tjänsten SkrivaPå. Dokumentet bifogas med detta mail.<%"\n"%></p>
                                                </span>
                       
remindMailNotSignedStandardHeader::(Monad m) => Context -> Document -> SignatoryLink -> (HSPT m XML) 
remindMailNotSignedStandardHeader ctx@Context{ctxmaybeuser = Just user} document signlink =   
                                                <span>
                                                 <p>Hej <%(personname signlink)%><%"\n"%><%"\n"%></p>
                                                 
                                                 <p><%(signatoryname $ documentauthordetails document)%> vill påminna dig om att du inte undertecknat dokument <% documenttitle document %> ännu.<%"\n"%><%"\n"%></p>  
                                               </span>

mailDocumentRejectedForAuthor :: (Maybe BS.ByteString) -> Context
                   -> BS.ByteString
                   -> BS.ByteString
                   -> Document
                   -> BS.ByteString
                   -> IO Mail
mailDocumentRejectedForAuthor customMessage 
                   ctx@(Context {ctxhostpart}) 
                   emailaddress personname 
                   document@Document{documenttitle,documentid} 
                           rejectorName = do
     let title = BS.append (BS.fromString "Avvisat: ")  documenttitle
     content <- htmlHeadBodyWrapIO documenttitle $ rejectMailContent customMessage ctx emailaddress personname document rejectorName
     return $ emptyMail {title = title, content = content}

rejectMailContent customMessage (Context {ctxhostpart}) 
                   emailaddress personname 
                   document@Document{documenttitle,documentid} 
                   rejectorName =   
     let link = ctxhostpart ++ show (LinkIssueDoc document)
         content = 
          <span>
            <p>Hej <% personname %>,</p>
            <p><% rejectorName %> har nekat att underteckna dokumentet <strong><% documenttitle %></strong>. 
               Avtalsprocessen är därmed avbruten.</p>
               <% poweredBySkrivaPaPara ctxhostpart %>
          </span>  
     in makeEditable "customtext" $ withCustom customMessage content        



mailInvitationToSignContent :: (Monad m) => Bool -> Context
               -> Document
               -> (Maybe SignatoryLink)
               -> (HSPT m XML)        
mailInvitationToSignContent forMail (Context {ctxmaybeuser = Just user, ctxhostpart}) 
                  document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
                  signaturelink = 
    let link = case (signaturelink) of
                Just signaturelink -> ctxhostpart ++ show (LinkSignDoc document signaturelink)
                Nothing -> ctxhostpart ++ "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        creatorname = signatoryname documentauthordetails
        common = <span>
                  <p><i>Översiktlig information</i><br/>
                   Parter: <% partyListString document %><br/>
                   Har undertecknat: <strong><% creatorname %></strong><br/>
                  <% case documenttimeouttime of 
                      Just time -> <span>Undertecknas senast: <strong><% show time %></strong>.</span>
                      Nothing -> <span></span>
                  %> 
                  </p>
                 <p><i>Så här går du vidare</i><br/>
                  1. Klicka på länken<br/>
                    <a href=link><% link %></a><br/>
                  2. Granska online<br/>
                  3. Underteckna<br/>
                  4. Färdig<br/>
                 </p>
                </span>
        defaultHeader = <p>Hej, <BR/> <%creatorname%> har bjudit in dig att underteckna dokumentet Testavtal online via tjänsten SkrivaPå. </p>
        defaultFooter = poweredBySkrivaPaPara ctxhostpart
        customFooter =  <p>
                         <small>
                          Hälsningar
                          <br/>
                          <% creatorname %>
                        </small> 
                        </p>
        customMessage = if (BS.null documentinvitetext) then Nothing else (Just documentinvitetext )       
        footer =    if (BS.null documentinvitetext) then (if (forMail) then defaultFooter else replaceOnEdit defaultFooter customFooter ) else customFooter 
        header   = withCustom customMessage (defaultHeader)
                   
   in  (makeEditable "customtext" header) `before` common `before` footer                     
   
        
mailInvitationToSign:: Context
               -> Document
               -> SignatoryLink 
               -> IO Mail
mailInvitationToSign ctx@(Context {ctxmaybeuser = Just user, ctxhostpart}) 
                  document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
                  signaturelink = 
               let title =  BS.concat [BS.fromString "Inbjudan från ",creatorname , BS.fromString " till ", documenttitle]
                   creatorname = signatoryname documentauthordetails
                   content = mailInvitationToSignContent True ctx document (Just signaturelink)
                in do  
                   content <- htmlHeadBodyWrapIO documenttitle content
                   return $ emptyMail {title = title, content = content}
      
    
mailDocumentClosedForSignatories :: Context
           -> BS.ByteString
           -> BS.ByteString
           -> Document
           -> SignatoryLink
           -> MagicHash
           -> IO Mail
mailDocumentClosedForSignatories (Context {ctxhostpart}) 
              emailaddress personname 
              document@Document{documenttitle,documentid} 
              signaturelink magichash = do
    let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle    
    let link = ctxhostpart ++ show (LinkSignDoc document signaturelink)
    content <- htmlHeadBodyWrapIO documenttitle 
         <span>
           <p>Hej <strong><% personname %></strong>,</p>
           <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats 
             av <% partyListString document %>. Avtalet är nu bindande.</p> 
          
           <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
   
           <% poweredBySkrivaPaPara ctxhostpart %>
         </span>
    return $ emptyMail {title = title, content = content}

mailDocumentClosedForAuthor :: Context
                 -> BS.ByteString
                 -> BS.ByteString
                 -> Document
                 -> IO Mail
mailDocumentClosedForAuthor (Context {ctxhostpart}) 
                  emailaddress personname 
                  document@Document{documenttitle,documentid} = 
    do
     let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle
     let link = ctxhostpart ++ show (LinkIssueDoc document)
     content <- htmlHeadBodyWrapIO documenttitle
        <span>
          <p>Hej <strong><% personname %></strong>,</p>
             <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats 
                av <% partyListString document %>. Avtalet är nu bindande.</p> 

             <p>Det färdigställda dokumentet bifogas med detta mail.</p> 

             <% poweredBySkrivaPaPara ctxhostpart %>
        </span> 
     return $ emptyMail {title = title, content = content}
