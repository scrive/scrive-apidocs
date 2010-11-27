{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module DocViewMail ( remindMail,
                     remindMailContent,
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

remindMail:: Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMail cm c d s = case s of 
                       SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSigned cm c d s
                       _ -> remindMailSigned cm c d s
                       
remindMailContent :: Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink ->IO String
remindMailContent cm c d s = case s of 
                               SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s
                               _ -> remindMailSignedContent cm c d s
                       
remindMailNotSigned::Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMailNotSigned customMessage ctx document@Document{documenttitle} signlink = 
       do
        title<- renderTemplate "remindMailNotSignedTitle" [("personname",BS.toString $ personname signlink)]
        content<- wrapHTML =<< remindMailNotSignedContent True customMessage ctx  document signlink  
        attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ documentfiles document          
        return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

        
        
remindMailSigned::Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned customMessage ctx document@Document{documenttitle}  signlink = 
                    do
                     let files = if (null $ documentsealedfiles document) then (documentfiles document) else (documentsealedfiles document)
                     title<- renderTemplate "remindMailNotSignedTitle" [("personname",BS.toString $ personname signlink)]
                     content<-wrapHTML =<<remindMailSignedContent customMessage ctx  document signlink     
                     attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ files   
                     return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

remindMailNotSignedContent:: Bool ->  (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> IO String                                
remindMailNotSignedContent forMail customMessage ctx  document signlink =  
               let link =  if (forMail)
                             then (ctxhostpart ctx) ++ show (LinkSignDoc document signlink)
                             else (ctxhostpart ctx) ++ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
                   creatorname = BS.toString $ personname' $ documentauthordetails document  
                   timetosigninfo = case (documenttimeouttime document) of 
                                     Just time -> renderTemplate "timetosigninfo" [("time",show time )]
                                     Nothing -> return ""
                   partnersinfo = renderListTemplate $  map (BS.toString . personname') $ partyList document       
                   footer =    if (forMail) 
                               then if (isNothing customMessage)
                                    then renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                                    else renderTemplate "customFooter" [("creatorname", creatorname)] 
                               else do
                                     this <- renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                                     with <- renderTemplate "customFooter" [("creatorname",creatorname)] 
                                     replaceOnEdit' this with                 
                   header   = if (isNothing customMessage) 
                              then remindMailNotSignedStandardHeader document signlink
                              else return $ BS.toString $ fromJust customMessage
                              
                in do
                    header' <- header
                    editableHeader <- makeEditable' "customtext" header'
                    footer' <- footer
                    partnersinfo' <- partnersinfo
                    timetosigninfo' <- timetosigninfo
                    renderTemplate "remindMailNotSignedContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("timetosigninfo",timetosigninfo'),
                                                                ("partnersinfo",partnersinfo'),  
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document),
                                                                ("link",link)]         

remindMailSignedContent ::(Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> IO String        
remindMailSignedContent customMessage ctx  document signlink =       
                           do
                           content <- if (isNothing customMessage) 
                                       then do   
                                            header <- remindMailSignedStandardHeader document signlink
                                            skv <- renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                                            return $ header ++skv
                                       else return $ BS.toString $ fromJust customMessage     
                           makeEditable' "customtext" content
                                          
                                      
                                      
                                      
remindMailSignedStandardHeader:: Document -> SignatoryLink -> IO String
remindMailSignedStandardHeader document signlink = renderTemplate "remindMailSignedStandardHeader" 
                                                               [("documenttitle",BS.toString $ documenttitle document),
                                                                ("author",BS.toString $ personname' $ documentauthordetails document),
                                                                ("personname",BS.toString $ personname signlink) ]
                                               
remindMailNotSignedStandardHeader:: Document -> SignatoryLink -> IO String
remindMailNotSignedStandardHeader document signlink =  renderTemplate "remindMailNotSignedStandardHeader" 
                                                               [("documenttitle",BS.toString $ documenttitle document),
                                                                ("author",BS.toString $ personname' $ documentauthordetails document),
                                                                ("personname",BS.toString $ personname signlink) ]
                                                                             
mailDocumentRejectedForAuthor ::(Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO Mail 
mailDocumentRejectedForAuthor customMessage ctx authorname  document@Document{documenttitle}  rejectorName = 
    do
     title <- renderTemplate "mailRejectMailTitle"  [("documenttitle",BS.toString documenttitle)]
     content <- wrapHTML =<< mailRejectMailContent customMessage ctx authorname document rejectorName
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent:: (Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO String  
mailRejectMailContent customMessage ctx  authorname  document  rejectorName =   
      do
       c<-case customMessage of
           Just message -> return $ BS.toString message
           Nothing -> renderTemplate "mailRejectMailContent" [("authorname",BS.toString $ authorname ),
                                                              ("documenttitle", BS.toString $ documenttitle document),
                                                              ("rejectorName",BS.toString rejectorName),  
                                                              ("ctxhostpart",ctxhostpart ctx)] 
       makeEditable' "customtext" c                                                                         


mailInvitationToSignContent :: Bool -> Context  -> Document  -> (Maybe SignatoryLink) -> IO String        
mailInvitationToSignContent forMail (Context {ctxhostpart}) 
                  document@Document{documenttimeouttime,documentauthordetails,documentinvitetext} 
                  signaturelink = 
    let link = case (signaturelink) of
                Just signaturelink' -> ctxhostpart ++ show (LinkSignDoc document signaturelink')
                Nothing -> ctxhostpart ++ "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        creatorname = BS.toString $ personname' documentauthordetails
        partnersinfo = if (forMail) 
                        then  renderListTemplate $  map (BS.toString . personname') $ partyList document
                        else  renderTemplate "updateinglistwithauthor" [("creatorname",creatorname )]
        timetosigninfo = case documenttimeouttime of 
                          Just time -> renderTemplate "timetosigninfo" [("time",show time )]
                          Nothing -> return ""
                          
        footer =    if (forMail) 
                    then if (BS.null documentinvitetext)
                           then renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart)] 
                           else renderTemplate "customFooter" [("creatorname", creatorname)] 
                    else do
                          this <- renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart)] 
                          with <- renderTemplate "customFooter" [("creatorname",creatorname)] 
                          replaceOnEdit' this with            
        header   =  if (BS.null documentinvitetext) 
                     then renderTemplate "mailInvitationToSignDefaultHeader" [("creatorname",creatorname)]  
                     else return $ BS.toString documentinvitetext      
           
   in  do
            header' <- header
            editableHeader <- makeEditable' "customtext" header'
            footer' <- footer
            partnersinfo' <- partnersinfo
            timetosigninfo' <- timetosigninfo
            renderTemplate "mailInvitationToSignContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("timetosigninfo",timetosigninfo'),
                                                                ("partnersinfo",partnersinfo'),  
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document),
                                                                ("link",link)]                      

        
mailInvitationToSign:: Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSign ctx document@Document{documenttitle,documentauthordetails} signaturelink = 
                 do  
                   title <- renderTemplate "mailInvitationToSignTitle" [("documenttitle",BS.toString  documenttitle ),
                                                                        ("creatorname",BS.toString $ personname' $ documentauthordetails)] 
                   content <- wrapHTML =<< mailInvitationToSignContent True ctx document (Just signaturelink)
                   return $ emptyMail {title = BS.fromString title, content = BS.fromString content}
      
    
mailDocumentClosedForSignatories :: Context -> Document -> SignatoryLink  -> IO Mail
mailDocumentClosedForSignatories (Context {ctxhostpart}) document@Document{documenttitle} signaturelink = 
   do
     title <- renderTemplate "mailDocumentClosedForSignatoriesTitle" [("documenttitle",BS.toString  documenttitle )] 
     partylist <- renderListTemplate $  map (BS.toString . personname') $ partyList document
     content <- wrapHTML =<< renderTemplate "mailDocumentClosedForSignatoriesContent" [("personname",BS.toString $ personname signaturelink ),
                                                                          ("documenttitle", BS.toString documenttitle ),
                                                                          ("partylist",partylist),  
                                                                          ("ctxhostpart",ctxhostpart)] 
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentClosedForAuthor :: Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentClosedForAuthor (Context {ctxhostpart}) authorname  document@Document{documenttitle} = 
    do
     title <- renderTemplate "mailDocumentAwaitingForAuthorTitle" [("documenttitle",BS.toString  documenttitle )] 
     partylist <- renderListTemplate $  map (BS.toString . personname') $ partyList document
     content <- renderTemplate "mailDocumentClosedForAuthorContent" [("authorname",BS.toString authorname),
                                                                     ("documenttitle", BS.toString documenttitle ),
                                                                     ("partylist",partylist),  
                                                                     ("ctxhostpart",ctxhostpart)] 
     content' <- wrapHTML content
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content'}


mailDocumentAwaitingForAuthor :: Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentAwaitingForAuthor (Context {ctxhostpart}) authorname  document@Document{documenttitle} =
    do
      title <- renderTemplate "mailDocumentAwaitingForAuthorTitle" [("documenttitle", BS.toString documenttitle )]
      content <- renderTemplate "mailDocumentAwaitingForAuthorContent" [("authorname",BS.toString authorname),
                                                                        ("documenttitle", BS.toString  documenttitle ),
                                                                        ("ctxhostpart",ctxhostpart),
                                                                        ("documentlink",ctxhostpart ++ (show $ LinkIssueDoc document))] 
      content' <- wrapHTML content
      return $ emptyMail {title = BS.fromString title, content = BS.fromString content'}

mailCancelDocumentByAuthorContent ::Bool ->  (Maybe BS.ByteString) -> Context  -> Document  -> IO String
mailCancelDocumentByAuthorContent forMail customMessage ctx document = 
        let 
        creatorname = BS.toString $ personname' $ documentauthordetails document 
        footer =    if (forMail) 
                    then if (isNothing customMessage)
                           then renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                           else renderTemplate "customFooter" [("creatorname", creatorname)] 
                    else do
                          this <- renderTemplate "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                          with <- renderTemplate "customFooter" [("creatorname",creatorname)] 
                          replaceOnEdit' this with
        header   = case customMessage of 
                     Just c -> return $ BS.toString c
                     Nothing  -> renderTemplate "mailCancelDocumentByAuthorStandardHeader" []
        in 
          do
            header' <- header
            editableHeader <- makeEditable' "customtext" header' 
            footer' <- footer
            renderTemplate "mailCancelDocumentByAuthorContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document)] 


mailCancelDocumentByAuthor ::(Maybe BS.ByteString) -> Context  -> Document -> SignatoryLink -> IO Mail 
mailCancelDocumentByAuthor customMessage ctx document@Document{documenttitle} signlink = 
       do
        title<- renderTemplate "mailCancelDocumentByAuthorTitle" [("documenttitle",BS.toString documenttitle)] 
        content <- wrapHTML =<< mailCancelDocumentByAuthorContent True customMessage ctx document    
        attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ documentfiles document          
        return $ emptyMail {title = BS.fromString title, fullnameemails =  [emailFromSignLink signlink] , content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}


makeEditable'::String->String->IO String
makeEditable' name this = renderTemplate "makeEditable" [("name",name),("this",this)]

replaceOnEdit'::String->String->IO String
replaceOnEdit' this with =  renderTemplate "replaceOnEdit" [("this",this),("with",with)]
