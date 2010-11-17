{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module DocViewMail ( remindMail,
                     remindMailContent,
                     mailDocumentRejectedForAuthor,
                     rejectMailContent,
                     mailDocumentClosedForAuthor, 
                     mailDocumentClosedForSignatories,
                     mailInvitationToSign,
                     mailInvitationToSignContent,
                     mailDocumentAwaitingForAuthor
           ) where
import AppView
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import User
import KontraLink
import Data.Maybe
import SendMail(Mail,emptyMail,content,title,attachments)
import DocViewUtil

remindMail:: Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMail cm c d s = case s of 
                       SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSigned cm c d s
                       _ -> remindMailSigned cm c d s
                       
remindMailContent :: (Monad m) => Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink ->(HSPT m XML) 
remindMailContent cm c d s = case s of 
                               SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s
                               _ -> remindMailSignedContent cm c d s
                       
remindMailNotSigned::Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMailNotSigned customMessage ctx document@Document{documenttitle} signlink = 
      let content  = remindMailNotSignedContent True customMessage ctx  document signlink                 
          attachmentcontent = filepdf $ head $ documentfiles document          
          title =  BS.concat [BS.fromString "Hej ",personname signlink]  
      in 
       do
        content' <- htmlHeadBodyWrapIO documenttitle content
        return $ emptyMail {title = title, content = content', attachments = [(documenttitle,attachmentcontent)]}

        
        
remindMailSigned::Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned customMessage ctx document@Document{documenttitle}  signlink = 
                     let 
                         title =  BS.concat [BS.fromString "Hej ",personname signlink]
                         files = if (null sealedfiles) then unsealedfiles else sealedfiles
                                 where sealedfiles = documentsealedfiles document
                                       unsealedfiles = documentfiles document
                         attachmentcontent = filepdf $ head $ files   
                         content = remindMailSignedContent customMessage ctx  document signlink     
                        
                     in do
                       content' <- htmlHeadBodyWrapIO documenttitle content
                       return $ emptyMail {title = title, content = content', attachments = [(documenttitle,attachmentcontent)]}

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
                   header   = withCustom  customMessage (remindMailNotSignedStandardHeader document signlink)
                   in (makeEditable "customtext" header) `before` content `before` footer 

remindMailSignedContent ::(Monad m) => (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> (HSPT m XML)                  
remindMailSignedContent customMessage ctx  document signlink        
                         = makeEditable "customtext" $
                            withCustom 
                                customMessage $
                                (remindMailSignedStandardHeader document signlink) `before` (poweredBySkrivaPaPara (ctxhostpart ctx))                     
                                      
                                      
                                      
remindMailSignedStandardHeader::(Monad m) => Document -> SignatoryLink -> (HSPT m XML) 
remindMailSignedStandardHeader document signlink =        
                                               <span>
                                                 <p>Hej <%(personname signlink)%><%"\n"%><%"\n"%></p>
                                                 <p><%(signatoryname $ documentauthordetails document)%> har begärt att vi ska skicka dokumentet <% documenttitle document %> till dig som du har undertecknat via tjänsten SkrivaPå. Dokumentet bifogas med detta mail.<%"\n"%></p>
                                                </span>
                                               
remindMailNotSignedStandardHeader::(Monad m) => Document -> SignatoryLink -> (HSPT m XML) 
remindMailNotSignedStandardHeader document signlink =   
                                                <span>
                                                 <p>Hej <%(personname signlink)%><%"\n"%><%"\n"%></p>
                                                 <p><%(signatoryname $ documentauthordetails document)%> vill påminna dig om att du inte undertecknat dokument <% documenttitle document %> ännu.<%"\n"%><%"\n"%></p>  
                                               </span>
                                               
mailDocumentRejectedForAuthor ::(Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO Mail 
mailDocumentRejectedForAuthor customMessage ctx authorname  document@Document{documenttitle}  rejectorName = 
    do
     let title = BS.append (BS.fromString "Avvisat: ")  documenttitle
     content <- htmlHeadBodyWrapIO documenttitle $ rejectMailContent customMessage ctx authorname document rejectorName
     return $ emptyMail {title = title, content = content}

rejectMailContent:: (Monad m) => (Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> (HSPT m XML)   
rejectMailContent customMessage ctx  authorname  document  rejectorName =   
      makeEditable "customtext" $ withCustom customMessage $
                    <span>
                      <p>Hej <% authorname %>,</p>
                      <p><% rejectorName %> har nekat att underteckna dokumentet <strong><%(documenttitle document)%></strong>. 
                      Avtalsprocessen är därmed avbruten.</p>
                      <% poweredBySkrivaPaPara (ctxhostpart ctx) %>
                    </span>       


mailInvitationToSignContent :: (Monad m) => Bool 
               -> Context
               -> Document
               -> (Maybe SignatoryLink)
               -> (HSPT m XML)        
mailInvitationToSignContent forMail (Context {ctxhostpart}) 
                  document@Document{documenttimeouttime,documentauthordetails,documentinvitetext} 
                  signaturelink = 
    let link = case (signaturelink) of
                Just signaturelink' -> ctxhostpart ++ show (LinkSignDoc document signaturelink')
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
        footer =    if (BS.null documentinvitetext) 
                    then (if (forMail)
                           then defaultFooter 
                           else replaceOnEdit defaultFooter customFooter) 
                    else customFooter 
        header   = withCustom customMessage (defaultHeader)
                   
   in  (makeEditable "customtext" header) `before` common `before` footer                     
   
        
mailInvitationToSign:: Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSign ctx document@Document{documenttitle,documentauthordetails} signaturelink = 
               let title =  BS.concat [BS.fromString "Inbjudan från ",creatorname , BS.fromString " till ", documenttitle]
                   creatorname = signatoryname documentauthordetails
                   content = mailInvitationToSignContent True ctx document (Just signaturelink)
                in do  
                   content' <- htmlHeadBodyWrapIO documenttitle content
                   return $ emptyMail {title = title, content = content'}
      
    
mailDocumentClosedForSignatories :: Context -> Document -> SignatoryLink  -> IO Mail
mailDocumentClosedForSignatories (Context {ctxhostpart}) document@Document{documenttitle} signaturelink = 
   do
    let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle    
    content' <- htmlHeadBodyWrapIO documenttitle 
         <span>
           <p>Hej <strong><% personname signaturelink %></strong>,</p>
           <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats 
             av <% partyListString document %>. Avtalet är nu bindande.</p> 
           <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
           <% poweredBySkrivaPaPara ctxhostpart %>
         </span>
    return $ emptyMail {title = title, content = content'}

mailDocumentClosedForAuthor :: Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentClosedForAuthor (Context {ctxhostpart}) authorname  document@Document{documenttitle} = 
    do
     let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle 
     content <- htmlHeadBodyWrapIO documenttitle
        <span>
          <p>Hej <strong><% authorname %></strong>,</p>
             <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats 
                av <% partyListString document %>. Avtalet är nu bindande.</p> 
             <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
             <% poweredBySkrivaPaPara ctxhostpart %>
        </span> 
     return $ emptyMail {title = title, content = content}

mailDocumentAwaitingForAuthor :: Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentAwaitingForAuthor (Context {ctxhostpart}) authorname  document@Document{documenttitle} = 
    -- FIXME: change to swedish
    do
     let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle 
     content <- htmlHeadBodyWrapIO documenttitle
        <span>
          <p>Hej <strong><% authorname %></strong>,</p>
             <p>Dokumentet <strong><% documenttitle %></strong> has been signed by all signatories. There were fields for the signatories to fill out. The contract is being held for your final review.  Please follow the link below to read and confirm the information for each signatory.</p>
             <a href=(ctxhostpart ++ (show $ LinkIssueDoc document))><% documenttitle %></a>

             <% poweredBySkrivaPaPara ctxhostpart %>
        </span> 
     return $ emptyMail {title = title, content = content}
