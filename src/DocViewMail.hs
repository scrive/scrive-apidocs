{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module DocViewMail ( mailDocumentRemind,
                     mailDocumentRemindContent,
                     mailDocumentRejectedForAuthor,
                     mailRejectMailContent,
                     mailDocumentClosedForAuthor, 
                     mailDocumentClosedForSignatories,
                     mailInvitationToSign,
                     mailInvitationToSignContent,
                     mailDocumentAwaitingForAuthor,
                     mailCancelDocumentByAuthorContent,
		             mailCancelDocumentByAuthor
           ) where
import DocState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import User
import KontraLink
import Data.Maybe
import SendMail(Mail,emptyMail,content,title,attachments,fullnameemails)
import DocViewUtil
import Amazon
import Templates

mailDocumentRemind::  KontrakcjaTemplates -> Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
mailDocumentRemind templates cm c d s = case s of 
                       SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSigned templates cm c d s
                       _ -> remindMailSigned templates cm c d s
                       
mailDocumentRemindContent ::  KontrakcjaTemplates -> Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink ->IO String
mailDocumentRemindContent templates cm c d s = case s of 
                               SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSignedContent templates False cm c d s
                               _ -> remindMailSignedContent templates cm c d s
                       
remindMailNotSigned:: KontrakcjaTemplates -> Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMailNotSigned templates customMessage ctx document@Document{documenttitle} signlink = 
       do
        title<- renderTemplate templates "remindMailNotSignedTitle" [("personname",BS.toString $ personname signlink)]
        content<- wrapHTML templates =<< remindMailNotSignedContent templates True customMessage ctx  document signlink  
        attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ documentfiles document          
        return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

        
        
remindMailSigned:: KontrakcjaTemplates -> Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned templates customMessage ctx document@Document{documenttitle}  signlink = 
                    do
                     let files = if (null $ documentsealedfiles document) then (documentfiles document) else (documentsealedfiles document)
                     title<- renderTemplate templates "remindMailNotSignedTitle" [("personname",BS.toString $ personname signlink)]
                     content<-wrapHTML templates =<<remindMailSignedContent templates customMessage ctx  document signlink     
                     attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ files   
                     return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

remindMailNotSignedContent:: KontrakcjaTemplates ->  Bool ->  (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> IO String                                
remindMailNotSignedContent templates forMail customMessage ctx  document signlink =  
               let link =  if (forMail)
                             then (ctxhostpart ctx) ++ show (LinkSignDoc document signlink)
                             else (ctxhostpart ctx) ++ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
                   creatorname = BS.toString $ personname' $ documentauthordetails document  
                   timetosigninfo = case (documenttimeouttime document) of 
                                     Just time -> renderTemplate templates "timetosigninfo" [("time",show time )]
                                     Nothing -> return ""
                   partnersinfo = renderListTemplate templates $  map (BS.toString . personname') $ partyList document       
                   footer =    if (forMail) 
                               then if (isNothing customMessage)
                                    then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                                    else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                               else do
                                     this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                                     with <- renderTemplate templates "customFooter" [("creatorname",creatorname)] 
                                     replaceOnEdit' templates this with                 
                   header   = if (isNothing customMessage) 
                              then remindMailNotSignedStandardHeader templates document signlink
                              else return $ BS.toString $ fromJust customMessage
                              
                in do
                    header' <- header
                    editableHeader <- makeEditable' templates "customtext" header'
                    footer' <- footer
                    partnersinfo' <- partnersinfo
                    timetosigninfo' <- timetosigninfo
                    renderTemplate templates "remindMailNotSignedContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("timetosigninfo",timetosigninfo'),
                                                                ("partnersinfo",partnersinfo'),  
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document),
                                                                ("link",link)]         

remindMailSignedContent :: KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> IO String        
remindMailSignedContent templates customMessage ctx  document signlink =       
                           do
                           content <- if (isNothing customMessage) 
                                       then do   
                                            header <- remindMailSignedStandardHeader templates document signlink
                                            skv <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                                            return $ header ++skv
                                       else return $ BS.toString $ fromJust customMessage     
                           makeEditable' templates "customtext" content
                                          
                                      
                                      
                                      
remindMailSignedStandardHeader::  KontrakcjaTemplates -> Document -> SignatoryLink -> IO String
remindMailSignedStandardHeader templates document signlink = renderTemplate templates "remindMailSignedStandardHeader" 
                                                               [("documenttitle",BS.toString $ documenttitle document),
                                                                ("author",BS.toString $ personname' $ documentauthordetails document),
                                                                ("personname",BS.toString $ personname signlink) ]
                                               
remindMailNotSignedStandardHeader::  KontrakcjaTemplates -> Document -> SignatoryLink -> IO String
remindMailNotSignedStandardHeader templates document signlink =  renderTemplate templates "remindMailNotSignedStandardHeader" 
                                                               [("documenttitle",BS.toString $ documenttitle document),
                                                                ("author",BS.toString $ personname' $ documentauthordetails document),
                                                                ("personname",BS.toString $ personname signlink) ]
                                                                             
mailDocumentRejectedForAuthor :: KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO Mail 
mailDocumentRejectedForAuthor templates customMessage ctx authorname  document@Document{documenttitle}  rejectorName = 
    do
     title <- renderTemplate templates "mailRejectMailTitle"  [("documenttitle",BS.toString documenttitle)]
     content <- wrapHTML templates =<< mailRejectMailContent templates customMessage ctx authorname document rejectorName
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent::  KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO String  
mailRejectMailContent templates customMessage ctx  authorname  document  rejectorName =   
      do
       c<-case customMessage of
           Just message -> return $ BS.toString message
           Nothing -> renderTemplate templates "mailRejectMailContent" [("authorname",BS.toString $ authorname ),
                                                              ("documenttitle", BS.toString $ documenttitle document),
                                                              ("rejectorName",BS.toString rejectorName),  
                                                              ("ctxhostpart",ctxhostpart ctx)] 
       makeEditable' templates "customtext" c                                                                         


mailInvitationToSignContent ::  KontrakcjaTemplates -> Bool -> Context  -> Document  -> (Maybe SignatoryLink) -> IO String        
mailInvitationToSignContent templates forMail (Context {ctxhostpart}) 
                  document@Document{documenttimeouttime,documentauthordetails,documentinvitetext} 
                  signaturelink = 
    let link = case (signaturelink) of
                Just signaturelink' -> ctxhostpart ++ show (LinkSignDoc document signaturelink')
                Nothing -> ctxhostpart ++ "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        personname1 = maybe "" (BS.toString . signatoryname . signatorydetails) signaturelink
        creatorname = BS.toString $ personname' documentauthordetails
        partnersinfo = if (forMail) 
                        then  renderListTemplate templates $  map (BS.toString . personname') $ partyList document
                        else  renderTemplate templates "updateinglistwithauthor" [("creatorname",creatorname )]
        timetosigninfo = case documenttimeouttime of 
                          Just time -> renderTemplate templates "timetosigninfo" [("time",show time )]
                          Nothing -> return ""
                          
        footer =    if (forMail) 
                    then if (BS.null documentinvitetext)
                           then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart)] 
                           else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                    else do
                          this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart)] 
                          with <- renderTemplate templates "customFooter" [("creatorname",creatorname)] 
                          replaceOnEdit' templates this with            
        header   =  if (BS.null documentinvitetext) 
                     then renderTemplate templates "mailInvitationToSignDefaultHeader" [("creatorname",creatorname)
                                                                                       ,("personname",personname1)]  
                     else return $ BS.toString documentinvitetext      
           
   in  do
            header' <- header
            editableHeader <- makeEditable' templates "customtext" header'
            footer' <- footer
            partnersinfo' <- partnersinfo
            timetosigninfo' <- timetosigninfo
            renderTemplate templates "mailInvitationToSignContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("timetosigninfo",timetosigninfo'),
                                                                ("partnersinfo",partnersinfo'),  
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document),
                                                                ("link",link)]                      

        
mailInvitationToSign::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSign templates ctx document@Document{documenttitle,documentauthordetails} signaturelink = 
                 do  
                   title <- renderTemplate templates "mailInvitationToSignTitle" [("documenttitle",BS.toString  documenttitle ),
                                                                        ("creatorname",BS.toString $ personname' $ documentauthordetails)] 
                   content <- wrapHTML templates =<< mailInvitationToSignContent templates True ctx document (Just signaturelink)
                   return $ emptyMail {title = BS.fromString title, content = BS.fromString content}
      
    
mailDocumentClosedForSignatories ::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink  -> IO Mail
mailDocumentClosedForSignatories templates (Context {ctxhostpart}) document@Document{documenttitle} signaturelink = 
   do
     title <- renderTemplate templates "mailDocumentClosedForSignatoriesTitle" [("documenttitle",BS.toString  documenttitle )] 
     partylist <- renderListTemplate templates $  map (BS.toString . personname') $ partyList document
     content <- wrapHTML templates =<< renderTemplate templates "mailDocumentClosedForSignatoriesContent" [("personname",BS.toString $ personname signaturelink ),
                                                                          ("documenttitle", BS.toString documenttitle ),
                                                                          ("partylist",partylist),  
                                                                          ("ctxhostpart",ctxhostpart)] 
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentClosedForAuthor ::  KontrakcjaTemplates -> Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentClosedForAuthor templates (Context {ctxhostpart}) authorname  document@Document{documenttitle} = 
    do
     title <- renderTemplate templates "mailDocumentAwaitingForAuthorTitle" [("documenttitle",BS.toString  documenttitle )] 
     partylist <- renderListTemplate templates $  map (BS.toString . personname') $ partyList document
     content <- renderTemplate templates "mailDocumentClosedForAuthorContent" [("authorname",BS.toString authorname),
                                                                     ("documenttitle", BS.toString documenttitle ),
                                                                     ("partylist",partylist),  
                                                                     ("ctxhostpart",ctxhostpart)] 
     content' <- wrapHTML templates content
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content'}


mailDocumentAwaitingForAuthor ::  KontrakcjaTemplates -> Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentAwaitingForAuthor templates (Context {ctxhostpart}) authorname  Document{documenttitle,documentid} =
    do
      title <- renderTemplate templates "mailDocumentAwaitingForAuthorTitle" [("documenttitle", BS.toString documenttitle )]
      content <- renderTemplate templates "mailDocumentAwaitingForAuthorContent" [("authorname",BS.toString authorname),
                                                                        ("documenttitle", BS.toString  documenttitle ),
                                                                        ("ctxhostpart",ctxhostpart),
                                                                        ("documentlink",ctxhostpart ++ (show $ LinkIssueDoc documentid))] 
      content' <- wrapHTML templates content
      return $ emptyMail {title = BS.fromString title, content = BS.fromString content'}

mailCancelDocumentByAuthorContent :: KontrakcjaTemplates -> Bool ->  (Maybe BS.ByteString) -> Context  -> Document  -> IO String
mailCancelDocumentByAuthorContent templates forMail customMessage ctx document = 
        let 
        creatorname = BS.toString $ personname' $ documentauthordetails document 
        footer =    if (forMail) 
                    then if (isNothing customMessage)
                           then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                           else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                    else do
                          this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                          with <- renderTemplate templates "customFooter" [("creatorname",creatorname)] 
                          replaceOnEdit' templates this with
        header   = case customMessage of 
                     Just c -> return $ BS.toString c
                     Nothing  -> renderTemplate templates "mailCancelDocumentByAuthorStandardHeader" []
        in 
          do
            header' <- header
            editableHeader <- makeEditable' templates "customtext" header' 
            footer' <- footer
            renderTemplate templates "mailCancelDocumentByAuthorContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document)] 


mailCancelDocumentByAuthor :: KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context  -> Document -> SignatoryLink -> IO Mail 
mailCancelDocumentByAuthor templates customMessage ctx document@Document{documenttitle} signlink = 
       do
        title<- renderTemplate templates "mailCancelDocumentByAuthorTitle" [("documenttitle",BS.toString documenttitle)] 
        content <- wrapHTML templates =<< mailCancelDocumentByAuthorContent templates True customMessage ctx document    
        attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ documentfiles document          
        return $ emptyMail {title = BS.fromString title, fullnameemails =  [emailFromSignLink signlink] , content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}


makeEditable':: KontrakcjaTemplates -> String->String->IO String
makeEditable' templates name this = renderTemplate templates "makeEditable" [("name",name),("this",this)]

replaceOnEdit':: KontrakcjaTemplates -> String->String->IO String
replaceOnEdit' templates this with =  renderTemplate templates "replaceOnEdit" [("this",this),("with",with)]
