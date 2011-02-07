{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module Doc.DocViewMail ( mailDocumentRemind,
                     mailDocumentRemindContent,
                     mailDocumentRejected,
                     mailRejectMailContent,
                     mailDocumentClosedForAuthor, 
                     mailDocumentClosedForSignatories,
                     mailInvitationToSign,
                     mailInvitationToSignContent,
                     mailInvitationToSend,
                     mailDocumentAwaitingForAuthor,
                     mailCancelDocumentByAuthorContent,
                     mailDocumentError,
                     mailCancelDocumentByAuthor
           ) where
import Doc.DocState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Kontra
import KontraLink
import Data.Maybe
import Mails.SendMail(Mail,emptyMail,content,title,attachments,fullnameemails)
import Doc.DocViewUtil
import Amazon
import Templates.Templates 
import Templates.TemplatesUtils
import User.UserView (prettyName)

mailDocumentRemind :: KontrakcjaTemplates 
                   -> Maybe (BS.ByteString) 
                   -> Context 
                   -> Document 
                   -> SignatoryLink 
                   -> User 
                   -> IO Mail
mailDocumentRemind templates cm c d s a = 
    case s of 
      SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned templates cm c d s a
      _                                       -> remindMailSigned    templates cm c d s a
                       
mailDocumentRemindContent :: KontrakcjaTemplates 
                          -> Maybe (BS.ByteString) 
                          -> Context 
                          -> Document 
                          -> SignatoryLink 
                          -> User
                          -> IO String
mailDocumentRemindContent templates cm c d s a = 
    case s of 
      SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent templates False cm c d s a
      _                                       -> remindMailSignedContent    templates cm c d s a
                       
remindMailNotSigned :: KontrakcjaTemplates 
                    -> Maybe (BS.ByteString) 
                    -> Context 
                    -> Document 
                    -> SignatoryLink 
                    -> User 
                    -> IO Mail
remindMailNotSigned templates customMessage ctx document@Document{documenttitle} signlink author = do
  title <- renderTemplate templates "remindMailNotSignedTitle" [("documenttitle",BS.toString $ documenttitle)]
  content <- wrapHTML templates =<< remindMailNotSignedContent templates True customMessage ctx  document signlink author
  attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ documentfiles document          
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

        
        
remindMailSigned:: KontrakcjaTemplates -> Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> User -> IO Mail
remindMailSigned templates customMessage ctx document@Document{documenttitle}  signlink author = 
                    do
                     let files = if (null $ documentsealedfiles document) then (documentfiles document) else (documentsealedfiles document)
                     title<- renderTemplate templates "remindMailSignedTitle" [("documenttitle",BS.toString $ documenttitle)]
                     content<-wrapHTML templates =<<remindMailSignedContent templates customMessage ctx  document signlink author
                     attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ files   
                     return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

remindMailNotSignedContent :: KontrakcjaTemplates 
                           ->  Bool 
                           ->  (Maybe BS.ByteString) 
                           -> Context 
                           -> Document 
                           -> SignatoryLink 
                           -> User 
                           -> IO String                                
remindMailNotSignedContent templates forMail customMessage ctx document signlink author =  
    let link = if (forMail)
                then (ctxhostpart ctx) ++ show (LinkSignDoc document signlink)
                else (ctxhostpart ctx) ++ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        creatorname = BS.toString $ prettyName author
        timetosigninfo = case (documenttimeouttime document) of 
                           Just time -> renderTemplate templates "timetosigninfo" [("time",show time )]
                           Nothing   -> return ""
        partnersinfo = renderListTemplate templates $  map (BS.toString . personname') $ partyList document    
        whohadsignedinfo =  do
                               signedlist <- if (not $ null $ partySignedList document)
                                                   then fmap Just $ renderListTemplate templates $  map (BS.toString . personname') $ partySignedList document
                                                   else return Nothing
                               renderTemplate templates "whohadsignedinfoformail" (setAttribute "signedlist" signedlist)

        footer =    if (forMail) 
                     then if (isNothing customMessage)
                           then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                           else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                     else do
                       this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                       with <- renderTemplate templates "customFooter" [("creatorname",creatorname)] 
                       replaceOnEdit' templates this with                 
        header   = if (isNothing customMessage) 
                    then remindMailNotSignedStandardHeader templates document signlink author
                    else return $ BS.toString $ fromJust customMessage
                              
    in do
      header' <- header
      editableHeader <- makeEditable' templates "customtext" header'
      footer' <- footer
      partnersinfo' <- partnersinfo
      whohadsignedinfo' <- whohadsignedinfo 
      timetosigninfo' <- timetosigninfo
      renderTemplate templates "remindMailNotSignedContent" [("header",editableHeader),
                                                             ("footer",footer'),
                                                             ("timetosigninfo",timetosigninfo'),
                                                             ("partnersinfo",partnersinfo'),  
                                                             ("whohadsignedinfo", whohadsignedinfo'),
                                                             ("creatorname", creatorname),
                                                             ("documenttitle",BS.toString $ documenttitle document),
                                                             ("link",link)]         

remindMailSignedContent :: KontrakcjaTemplates 
                        -> (Maybe BS.ByteString) 
                        -> Context 
                        -> Document 
                        -> SignatoryLink 
                        -> User
                        -> IO String        
remindMailSignedContent templates customMessage ctx document signlink author = do
  content <- if (isNothing customMessage) 
              then do   
                header <- remindMailSignedStandardHeader templates document signlink author
                skv <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart  ctx)] 
                return $ header ++ skv
              else return $ BS.toString $ fromJust customMessage     
  makeEditable' templates "customtext" content
                                      
remindMailSignedStandardHeader :: KontrakcjaTemplates 
                               -> Document 
                               -> SignatoryLink 
                               -> User 
                               -> IO String
remindMailSignedStandardHeader templates document signlink author = 
    renderTemplate templates "remindMailSignedStandardHeader" 
                       [("documenttitle", BS.toString $ documenttitle document),
                        ("author", BS.toString $ prettyName author),
                        ("personname", BS.toString $ personname signlink) ]
                                               
remindMailNotSignedStandardHeader::  KontrakcjaTemplates -> Document -> SignatoryLink -> User -> IO String
remindMailNotSignedStandardHeader templates document signlink author =  renderTemplate templates "remindMailNotSignedStandardHeader" 
                                                               [("documenttitle",BS.toString $ documenttitle document),
                                                                ("author",BS.toString $ prettyName author),
                                                                ("personname",BS.toString $ personname signlink) ]
                                                                             
mailDocumentRejected :: KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO Mail 
mailDocumentRejected templates customMessage ctx username  document@Document{documenttitle}  rejectorName = 
    do
     title <- renderTemplate templates "mailRejectMailTitle"  [("documenttitle",BS.toString documenttitle)]
     content <- wrapHTML templates =<< mailRejectMailContent templates customMessage ctx username document rejectorName
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent::  KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context -> BS.ByteString -> Document -> BS.ByteString -> IO String  
mailRejectMailContent templates customMessage ctx  username  document  rejectorName =   
      do
       c<-case customMessage of
           Just message -> return $ BS.toString message
           Nothing -> renderTemplate templates "mailRejectMailContent" [("username",BS.toString $ username ),
                                                              ("documenttitle", BS.toString $ documenttitle document),
                                                              ("rejectorName",BS.toString rejectorName),  
                                                              ("ctxhostpart",ctxhostpart ctx)] 
       makeEditable' templates "customtext" c                                                                         

mailDocumentError :: KontrakcjaTemplates -> Context -> Document -> IO Mail 
mailDocumentError templates ctx document@Document{documenttitle} = 
    do
     title <- renderTemplate templates "mailDocumentErrorTitle"  [("documenttitle",BS.toString documenttitle)]
     content <- wrapHTML templates =<< mailDocumentErrorContent templates ctx document
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}


mailDocumentErrorContent :: KontrakcjaTemplates -> Context -> Document -> IO String  
mailDocumentErrorContent templates ctx document =   
  -- FIXME: should have author name here also
  renderTemplate templates "mailDocumentErrorContent" [-- ("authorname", BS.toString $ authorname ),
    ("documenttitle", BS.toString $ documenttitle document),
    ("ctxhostpart", ctxhostpart ctx)] 

mailInvitationToSignContent ::  KontrakcjaTemplates -> Bool -> Context  -> Document -> User -> (Maybe SignatoryLink) -> IO String        
mailInvitationToSignContent templates forMail (Context {ctxhostpart}) 
                  document@Document{documenttimeouttime, documentinvitetext,documenttitle} 
                  author
                  signaturelink = 
    let link = case (signaturelink) of
                Just signaturelink' -> ctxhostpart ++ show (LinkSignDoc document signaturelink')
                Nothing -> ctxhostpart ++ "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        personname1 = maybe "" (BS.toString . signatoryname . signatorydetails) signaturelink
        creatorname = BS.toString $ prettyName author
        partnersinfo = if (forMail) 
                        then  renderListTemplate templates $  map (BS.toString . personname') $ partyList document
                        else  renderTemplate templates "updateinglistwithauthor" [("creatorname",creatorname )]
        whohadsignedinfo =  if (forMail) 
                             then do
                                    signedlist <- if (not $ null $ partySignedList document)
                                                       then fmap Just $ renderListTemplate templates $  map (BS.toString . personname') $ partySignedList document
                                                       else return Nothing
                                    renderTemplate templates "whohadsignedinfoformail" (setAttribute "signedlist" signedlist)
                             else  renderTemplate templates "whohadsignedinfoforpreview" ()
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
                                                                                       ,("personname",personname1)
                                                                                       ,("documenttitle",BS.toString documenttitle) 
                                                                                       ]  
                     else return $ BS.toString documentinvitetext      
           
   in  do
            header' <- header
            editableHeader <- makeEditable' templates "customtext" header'
            footer' <- footer
            partnersinfo' <- partnersinfo
            whohadsignedinfo' <- whohadsignedinfo
            timetosigninfo' <- timetosigninfo
            renderTemplate templates "mailInvitationToSignContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("timetosigninfo",timetosigninfo'),
                                                                ("partnersinfo",partnersinfo'),  
                                                                ("whohadsignedinfo", whohadsignedinfo'),
                                                                ("documenttitle",BS.toString documenttitle),
                                                                ("link",link)]    
                                                                
mailInvitationToSign::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> User -> IO Mail
mailInvitationToSign templates ctx document@Document{documenttitle} signaturelink author = 
                 do  
                   title <- renderTemplate templates "mailInvitationToSignTitle" [("documenttitle",BS.toString  documenttitle ),
                                                                        ("creatorname",BS.toString $ prettyName author)] 
                   content <- wrapHTML templates =<< mailInvitationToSignContent templates True ctx document author (Just signaturelink)
                   return $ emptyMail {title = BS.fromString title, content = BS.fromString content}


mailInvitationToSend::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> User -> IO Mail
mailInvitationToSend templates ctx document@Document{documenttitle} signaturelink author = 
                 do  
                   title <- renderTemplate templates "mailInvitationToSignTitle" [("documenttitle",BS.toString  documenttitle ),
                                                                        ("creatorname",BS.toString $ prettyName author)] 
                   content <- wrapHTML templates =<< mailInvitationToSignContent templates True ctx document author (Just signaturelink)
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

mailCancelDocumentByAuthorContent :: KontrakcjaTemplates -> Bool ->  (Maybe BS.ByteString) -> Context  -> Document -> User -> IO String
mailCancelDocumentByAuthorContent templates forMail customMessage ctx document author = 
        let 
        creatorname = BS.toString $ prettyName author
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
                     Nothing  -> renderTemplate templates "mailCancelDocumentByAuthorStandardHeader" ()
        in 
          do
            header' <- header
            editableHeader <- makeEditable' templates "customtext" header' 
            footer' <- footer
            renderTemplate templates "mailCancelDocumentByAuthorContent" [("header",editableHeader),
                                                                ("footer",footer'),
                                                                ("creatorname", creatorname),
                                                                ("documenttitle",BS.toString $ documenttitle document)] 


mailCancelDocumentByAuthor :: KontrakcjaTemplates -> (Maybe BS.ByteString) -> Context  -> Document -> User -> SignatoryLink -> IO Mail 
mailCancelDocumentByAuthor templates customMessage ctx document@Document{documenttitle} author signlink = 
       do
        title<- renderTemplate templates "mailCancelDocumentByAuthorTitle" [("documenttitle",BS.toString documenttitle)] 
        content <- wrapHTML templates =<< mailCancelDocumentByAuthorContent templates True customMessage ctx document author
        attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ documentfiles document          
        return $ emptyMail {title = BS.fromString title, fullnameemails =  [emailFromSignLink signlink] , content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}


makeEditable':: KontrakcjaTemplates -> String->String->IO String
makeEditable' templates name this = renderTemplate templates "makeEditable" [("name",name),("this",this)]

replaceOnEdit':: KontrakcjaTemplates -> String->String->IO String
replaceOnEdit' templates this with =  renderTemplate templates "replaceOnEdit" [("this",this),("with",with)]
