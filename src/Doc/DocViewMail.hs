{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module Doc.DocViewMail ( mailDocumentRemind,
                     mailDocumentRemindContent,
                     mailDocumentRejected,
                     mailRejectMailContent,
                     mailDocumentClosedForAuthor, 
                     mailDocumentClosedForSignatories,
                     mailInvitationToSign,
                     mailInvitationToSignOrViewContent,
                     mailInvitationToSend,
                     mailInvitationToView,
                     mailDocumentAwaitingForAuthor,
                     mailCancelDocumentByAuthorContent,
                     mailDocumentError,
                     mailCancelDocumentByAuthor,
                     mailMismatchSignatory,
                     mailMismatchAuthor
           ) where
import Doc.DocState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Kontra
import KontraLink
import Data.Maybe
import Mails.SendMail(Mail,emptyMail,content,title,fullnameemails,attachments)
import Doc.DocViewUtil
import Amazon
import Templates.Templates 
import Templates.TemplatesUtils
import User.UserView (prettyName)
import Misc

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
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

        
        
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
  header <- if (isNothing customMessage)
              then remindMailSignedStandardHeader templates document signlink author
              else return . BS.toString $ fromJust customMessage
  editableheader <- makeEditable' templates "customtext" header
  skv <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)]
  return $ editableheader ++ skv
      
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
                                                                             
mailDocumentRejected :: KontrakcjaTemplates -> (Maybe String) -> Context -> BS.ByteString -> Document -> SignatoryLink -> IO Mail 
mailDocumentRejected templates customMessage ctx username  document@Document{documenttitle}  rejector = 
    do
     title <- renderTemplate templates "mailRejectMailTitle"  [("documenttitle",BS.toString documenttitle)]
     content <- wrapHTML templates =<< mailRejectMailContent templates customMessage ctx username document rejector
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent::  KontrakcjaTemplates -> (Maybe String) -> Context -> BS.ByteString -> Document -> SignatoryLink -> IO String  
mailRejectMailContent templates customMessage ctx  username  document  rejector =   
      renderTemplate templates "mailRejectMailContent" $ do 
           field "username" username
           field "documenttitle" $ documenttitle document
           field "rejectorName" $ personname rejector
           field "ctxhostpart" $ ctxhostpart ctx
           field "customMessage" $ customMessage


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

mailInvitationToSignOrViewContent :: KontrakcjaTemplates 
                                  -> Bool 
                                  -> Context
                                  -> Document
                                  -> User
                                  -> Maybe SignatoryLink
                                  -> IO String        
mailInvitationToSignOrViewContent templates forMail (Context {ctxhostpart}) 
                  document@Document{documenttimeouttime, documentinvitetext,documenttitle} 
                  author
                  signaturelink = 
    let link = case (signaturelink) of
                Just signaturelink' -> ctxhostpart ++ show (LinkSignDoc document signaturelink')
                Nothing -> ctxhostpart ++ "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        issignatory = maybe True ((SignatoryPartner `elem`) . signatoryroles) signaturelink
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
            renderTemplate templates "mailInvitationToSignContent" $ do
                field "header" editableHeader
                field "footer" footer'
                field "timetosigninfo" timetosigninfo'
                field "partnersinfo" partnersinfo'
                field "whohadsignedinfo" whohadsignedinfo'
                field "documenttitle" $ BS.toString documenttitle
                field "offer" $ isOffer document
                field "link" link    
                field "issignatory" issignatory
                                                                
mailInvitationToSign::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> User -> IO Mail
mailInvitationToSign templates ctx document@Document{documenttitle} signaturelink author = 
                 do  
                   title <- renderTemplate templates "mailInvitationToSignTitle" [("documenttitle",BS.toString  documenttitle ),
                                                                        ("creatorname",BS.toString $ prettyName author)] 
                   content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document author (Just signaturelink)
                   return $ emptyMail {title = BS.fromString title, content = BS.fromString content}


mailInvitationToSend::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> User -> IO Mail
mailInvitationToSend templates ctx document@Document{documenttitle} signaturelink author = 
                 do  
                   title <- renderTemplate templates "mailInvitationToSignTitle" [("documenttitle",BS.toString  documenttitle ),
                                                                        ("creatorname",BS.toString $ prettyName author)] 
                   content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document author (Just signaturelink)
                   return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToView ::  KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> User -> IO Mail
mailInvitationToView templates ctx document@Document{documenttitle} signaturelink author = 
    do  
        title <- renderTemplate templates "mailInvitationToViewTitle" 
                 [("documenttitle",BS.toString  documenttitle ),
                  ("creatorname",BS.toString $ prettyName author)] 
        content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document author (Just signaturelink)
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
     title <- renderTemplate templates "mailDocumentClosedForAuthorTitle" [("documenttitle",BS.toString  documenttitle )] 
     partylist <- renderListTemplate templates $  map (BS.toString . personname') $ partyList document
     content <- renderTemplate templates "mailDocumentClosedForAuthorContent" [("authorname",BS.toString authorname),
                                                                     ("documenttitle", BS.toString documenttitle ),
                                                                     ("partylist",partylist),  
                                                                     ("ctxhostpart",ctxhostpart)] 
     content' <- wrapHTML templates content
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content'}


mailDocumentAwaitingForAuthor ::  KontrakcjaTemplates -> Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentAwaitingForAuthor templates (Context {ctxhostpart}) authorname  document@Document{documenttitle,documentid} =
    do
      let signatoriesSigned = partySignedList document
      signatories <- renderListTemplate templates $ map (BS.toString . personname') signatoriesSigned
      title <- renderTemplate templates "mailDocumentAwaitingForAuthorTitle" [("documenttitle", BS.toString documenttitle )]
      content <- renderTemplate templates "mailDocumentAwaitingForAuthorContent" [("authorname",BS.toString authorname),
                                                                        ("documenttitle", BS.toString  documenttitle ),
                                                                        ("ctxhostpart",ctxhostpart),
                                                                        ("documentlink",ctxhostpart ++ (show $ LinkIssueDoc documentid)),
                                                                        ("partylist", signatories)] 
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
        return $ emptyMail {title = BS.fromString title, fullnameemails =  [emailFromSignLink signlink] , content = BS.fromString content}

mailMismatchSignatory :: Context 
                        -> Document 
                        -> String 
                        -> String 
                        -> String 
                        -> String 
                        -> String
                        -> String 
                        -> Bool 
                        -> IO Mail
mailMismatchSignatory ctx document authoremail authorname doclink signame badname msg isbad = do
    let Context { ctxtemplates } = ctx
    title <- renderTemplate ctxtemplates "mailMismatchSignatoryTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
    content <- wrapHTML ctxtemplates =<< (renderTemplate ctxtemplates "mailMismatchSignatoryContent" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
        field "authorname" authorname
        field "signame" signame
        field "badname" badname
        field "authoremail" authoremail
        field "doclink" doclink
        field "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing))
        
    return $ emptyMail  { title = BS.fromString title
                        , content = BS.fromString content 
                        }
                        
mailMismatchAuthor :: Context -> Document -> String -> String -> String -> IO Mail
mailMismatchAuthor ctx document authorname badname bademail = do
    let Context { ctxtemplates } = ctx
        Just (ELegDataMismatch msg _ _ _ _) = documentcancelationreason document
    title <- renderTemplate ctxtemplates "mailMismatchAuthorTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
    content <- wrapHTML ctxtemplates =<< (renderTemplate ctxtemplates "mailMismatchAuthorContent" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
        field "messages" $ concat $ map para $ lines msg
        field "authorname" authorname
        field "doclink"  (ctxhostpart ctx ++ (show $ LinkDesignDoc (DesignStep2 (documentid document)
                                                            Nothing
                                                            Nothing)))
        field "bademail" bademail
        field "badname" badname)
    return $ emptyMail  { title = BS.fromString title
                        , content = BS.fromString content
                        }
        
makeEditable':: KontrakcjaTemplates -> String->String->IO String
makeEditable' templates name this = renderTemplate templates "makeEditable" [("name",name),("this",this)]

replaceOnEdit':: KontrakcjaTemplates -> String->String->IO String
replaceOnEdit' templates this with =  renderTemplate templates "replaceOnEdit" [("this",this),("with",with)]
