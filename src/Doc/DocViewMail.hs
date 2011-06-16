{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}

module Doc.DocViewMail ( mailDocumentRemind
                       , mailDocumentRemindContent
                       , mailDocumentRejected
                       , mailRejectMailContent
                       , mailDocumentClosed
                       , mailInvitationToSign
                       , mailInvitationToSignOrViewContent
                       , mailInvitationToSend
                       , mailInvitationToView
                       , mailDocumentAwaitingForAuthor
                       , mailCancelDocumentByAuthorContent
                       , mailDocumentError
                       , mailCancelDocumentByAuthor
                       , mailMismatchSignatory
                       , mailMismatchAuthor
                       ) where

import Amazon
import Doc.DocState
import Doc.DocUtils
import Doc.DocProcess
import KontraLink
import Mails.SendMail
import Misc
import Templates.Templates 
import Templates.TemplatesUtils
import Kontra
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import API.Service.ServiceState
import Happstack.State (query) 
import Templates.TemplatesLoader

mailDocumentRemind :: KontrakcjaTemplates 
                   -> Maybe (BS.ByteString) 
                   -> Context 
                   -> Document 
                   -> SignatoryLink 
                   -> IO Mail
mailDocumentRemind templates cm c d s = 
  case s of
    SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned templates cm c d s
    _                                       -> remindMailSigned    templates cm c d s
                       
mailDocumentRemindContent :: KontrakcjaTemplates 
                          -> Maybe (BS.ByteString) 
                          -> Context 
                          -> Document 
                          -> SignatoryLink 
                          -> IO String
mailDocumentRemindContent templates cm c d s = 
  case s of 
    SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent templates False cm c d s
    _                                       -> remindMailSignedContent    templates cm c d s
                       
remindMailNotSigned :: KontrakcjaTemplates 
                    -> Maybe (BS.ByteString) 
                    -> Context 
                    -> Document 
                    -> SignatoryLink 
                    -> IO Mail
remindMailNotSigned templates customMessage ctx document@Document{documenttitle} signlink = do
  title <- renderTemplate templates "remindMailNotSignedTitle" [("documenttitle",BS.toString $ documenttitle)]
  content <- wrapHTML templates =<< remindMailNotSignedContent templates True customMessage ctx  document signlink
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

        
        
remindMailSigned:: KontrakcjaTemplates -> Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned templates customMessage ctx document@Document{documenttitle}  signlink = do
  let files = if (null $ documentsealedfiles document) then (documentfiles document) else (documentsealedfiles document)
  title <- renderTemplate templates "remindMailSignedTitle" [("documenttitle",BS.toString $ documenttitle)]
  content <- wrapHTML templates =<<remindMailSignedContent templates customMessage ctx  document signlink
  attachmentcontent <- getFileContents (ctxs3action ctx) $ head $ files   
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content, attachments = [(documenttitle,attachmentcontent)]}

remindMailNotSignedContent :: KontrakcjaTemplates 
                           -> Bool 
                           -> Maybe BS.ByteString
                           -> Context 
                           -> Document 
                           -> SignatoryLink 
                           -> IO String                                
remindMailNotSignedContent templates forMail customMessage ctx document signlink =  
    let link = if (forMail)
                then (ctxhostpart ctx) ++ show (LinkSignDoc document signlink)
                else (ctxhostpart ctx) ++ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        mauthorsiglink = getAuthorSigLink document
        creatorname = maybe "" (BS.toString . personname) mauthorsiglink
        timetosigninfo = case (documenttimeouttime document) of 
                           Just time -> renderTemplate templates "timetosigninfo" [("time",show time )]
                           Nothing   -> return ""
        partnersinfo = renderListTemplate templates $  map (BS.toString . personname') $ partyList document    
        whohadsignedinfo =  do
                               signedlist <- if (not $ null $ partySignedList document)
                                                   then fmap Just $ renderListTemplate templates $  map (BS.toString . personname') $ partySignedList document
                                                   else return Nothing
                               renderTemplateForProcess templates document processwhohadsignedinfoformail $ do
                                   field "signedlist" signedlist

        footer =    if (forMail) 
                     then if (isNothing customMessage)
                           then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                           else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                     else do
                       this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)] 
                       with <- renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                       replaceOnEdit' templates this with                 
        header   = if (isNothing customMessage) 
                    then remindMailNotSignedStandardHeader templates document signlink
                    else return $ BS.toString $ fromJust customMessage
                              
    in do
      header' <- header
      editableHeader <- makeEditable' templates "customtext" header'
      footer' <- footer
      partnersinfo' <- partnersinfo
      whohadsignedinfo' <- whohadsignedinfo 
      timetosigninfo' <- timetosigninfo
      renderTemplateForProcess templates document processmailremindnotsignedcontent $ do
          field "header" editableHeader
          field "footer" $ footer'
          field "timetosigninfo" $ timetosigninfo'
          field "partnersinfo" $ partnersinfo'
          field "whohadsignedinfo" $  whohadsignedinfo'
          field "creatorname"  creatorname
          field "documenttitle" $ BS.toString $ documenttitle document
          field "link" $ link      

remindMailSignedContent :: KontrakcjaTemplates 
                        -> (Maybe BS.ByteString) 
                        -> Context 
                        -> Document 
                        -> SignatoryLink 
                        -> IO String        
remindMailSignedContent templates customMessage ctx document signlink = do
  header <- if (isNothing customMessage)
              then remindMailSignedStandardHeader templates document signlink
              else return . BS.toString $ fromJust customMessage
  editableheader <- makeEditable' templates "customtext" header
  skv <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart ctx)]
  return $ editableheader ++ skv
      
remindMailSignedStandardHeader :: KontrakcjaTemplates 
                               -> Document 
                               -> SignatoryLink 
                               -> IO String
remindMailSignedStandardHeader templates document signlink = 
  renderTemplateForProcess templates document processmailsignedstandardheader $ do 
    let mauthorsiglink = getAuthorSigLink document
        creatorname = maybe (BS.fromString "") personname mauthorsiglink
    field "documenttitle" $  BS.toString $ documenttitle document
    field "author" $ BS.toString creatorname
    field "personname" $ BS.toString $ personname signlink
                                               
remindMailNotSignedStandardHeader::  KontrakcjaTemplates -> Document -> SignatoryLink -> IO String
remindMailNotSignedStandardHeader templates document signlink =  
  renderTemplateForProcess templates document processmailnotsignedstandardheader $ do
    let mauthorsiglink = getAuthorSigLink document
        creatorname = maybe (BS.fromString "") personname mauthorsiglink
    field "documenttitle" $ BS.toString $ documenttitle document
    field "author" $ BS.toString creatorname
    field "personname" $ BS.toString $ personname signlink
                                                                             
mailDocumentRejected :: KontrakcjaTemplates -> (Maybe String) -> Context -> BS.ByteString -> Document -> SignatoryLink -> IO Mail 
mailDocumentRejected templates customMessage ctx username  document@Document{documenttitle}  rejector = 
    do
     title <- renderTemplate templates "mailRejectMailTitle"  [("documenttitle",BS.toString documenttitle)]
     content <- wrapHTML templates =<< mailRejectMailContent templates customMessage ctx username document rejector
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent::  KontrakcjaTemplates -> (Maybe String) -> Context -> BS.ByteString -> Document -> SignatoryLink -> IO String  
mailRejectMailContent templates customMessage ctx  username  document  rejector =   
      renderTemplateForProcess templates document processmailrejectcontent $ do 
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
                                  -> Maybe SignatoryLink
                                  -> IO String        
mailInvitationToSignOrViewContent templates 
                                  forMail 
                                  Context{ctxhostpart}
                                  document@Document{ documenttimeouttime, documentinvitetext, documenttitle }
                                  msiglink = do
  let link = case msiglink of
                Just siglink -> ctxhostpart ++ show (LinkSignDoc document siglink)
                Nothing -> ctxhostpart ++ "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
      Just authorsiglink = getAuthorSigLink document
      creatorname = BS.toString $ signatoryname $ signatorydetails authorsiglink
      issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
      personname1 = maybe "" (BS.toString . signatoryname . signatorydetails) msiglink
      partnersinfo = if forMail
                     then renderListTemplate templates $ map (BS.toString . personname') $ partyList document
                     else renderTemplate templates "updateinglistwithauthor" [("creatorname", creatorname )]
      whohadsignedinfo =  if (forMail) 
                             then do
                                    signedlist <- if (not $ null $ partySignedList document)
                                                       then fmap Just $ renderListTemplate templates $  map (BS.toString . personname') $ partySignedList document
                                                       else return Nothing
                                    renderTemplateForProcess templates document processwhohadsignedinfoformail $ do
                                        field "signedlist" signedlist
                             else  renderTemplate templates "whohadsignedinfoforpreview" [("asdf","asdf")]
      timetosigninfo = case documenttimeouttime of 
                          Just time -> renderTemplate templates "timetosigninfo" [("time",show time )]
                          Nothing -> return ""
                          
      footer =    if (forMail) 
                    then if (BS.null documentinvitetext)
                           then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart)] 
                           else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                    else do
                          this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart",ctxhostpart)] 
                          with <- renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                          replaceOnEdit' templates this with            
      header   =  if (BS.null documentinvitetext) 
                     then if issignatory || not forMail 
                          then renderTemplateForProcess templates document processmailinvitationtosigndefaultheader $ do
                                   field "creatorname" $ creatorname
                                   field"personname" $ personname1
                                   field "documenttitle" $ BS.toString documenttitle
                                     
                          else renderTemplate templates "mailInvitationToViewDefaultHeader" 
                                   [("creatorname",   creatorname)
                                   ,("personname",    personname1)
                                   ,("documenttitle", BS.toString documenttitle) 
                                   ]  
                     else return $ BS.toString documentinvitetext      
      sigattachments = for (buildattach document (documentsignatoryattachments document) [])
                       (\(n, _, sigs) -> (n, renderListTemplate templates (map (BS.toString . fst) sigs)))
           
  header' <- header
  editableHeader <- makeEditable' templates "customtext" header'
  footer' <- footer
  partnersinfo' <- partnersinfo
  whohadsignedinfo' <- whohadsignedinfo
  timetosigninfo' <- timetosigninfo
  renderTemplateForProcess templates document processmailinvitationtosigncontent $ do
        field "header" editableHeader
        field "footer" footer'
        field "timetosigninfo" timetosigninfo'
        field "partnersinfo" partnersinfo'
        field "whohadsignedinfo" whohadsignedinfo'
        field "documenttitle" $ BS.toString documenttitle
        field "link" link    
        field "issignatory" $ issignatory || not forMail 
        field "creatorname" creatorname
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename . authorattachmentfile) (documentauthorattachments document)
        field "hassigattachments" $ length (documentsignatoryattachments document ) > 0
        field "sigattachments" $ 
          for  sigattachments (\(n, sigs) -> do
                                  field "name" n
                                  field "sigs" sigs)


mailInvitationFromService :: Context -> Service -> Document -> SignatoryLink -> IO Mail
mailInvitationFromService ctx service doc sl = do
  templates <- toKontrakcjaTemplates [("invitationmail",BS.toString $ servicedocumentinvitationmail service)]
  content <- renderTemplate templates "invitationmail" $ do
    field "documenttitle" $ BS.toString  $ documenttitle doc
    field "documentid" $ show $ documentid doc 
    field "documentlink" $ (ctxhostpart ctx)++ show (LinkSignDoc doc sl)       
    field "email" $ signatoryemail $ signatorydetails $ sl
  htmlContent <- wrapHTML (ctxtemplates ctx) $ content++"\n"
  return $ emptyMail {title = BS.fromString "Invitation", content = BS.fromString htmlContent}

mailInvitationToSign :: KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSign templates ctx doc sl = do
  mservice <- liftMM (query . GetService) (return $ getService doc)
  case (mservice) of
    Nothing -> mailInvitationToSign' templates ctx doc sl
    Just service -> mailInvitationFromService ctx service doc sl
      
mailInvitationToSend :: KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSend templates ctx doc sl = do
  mservice <- liftMM (query . GetService) (return $ getService doc)
  case (mservice) of
    Nothing -> mailInvitationToSend' templates ctx doc sl
    Just service -> mailInvitationFromService ctx service doc sl

mailInvitationToView :: KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail    
mailInvitationToView templates ctx doc sl  = do
    mservice <- liftMM (query . GetService) (return $ getService doc)
    case (mservice) of
      Nothing -> mailInvitationToView' templates ctx doc sl
      Just service -> mailInvitationFromService ctx service doc sl

mailInvitationToSign' :: KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSign' templates ctx document@Document { documenttitle } siglink = do
  let Just authorsiglink = getAuthorSigLink document
      authorname = signatoryname  $ signatorydetails authorsiglink
  title <- renderTemplate templates "mailInvitationToSignTitle" [ ("documenttitle", BS.toString documenttitle)
                                                                , ("creatorname",   BS.toString authorname)
                                                                ] 
  content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document (Just siglink)
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToSend' :: KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToSend' templates ctx document@Document{documenttitle} signaturelink = do
  let Just authorsiglink = getAuthorSigLink document
      authorname = signatoryname  $ signatorydetails authorsiglink
  title <- renderTemplate templates "mailInvitationToSignTitle" [("documenttitle", BS.toString documenttitle),
                                                                 ("creatorname",   BS.toString authorname)]
  content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document (Just signaturelink)
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToView' :: KontrakcjaTemplates -> Context -> Document -> SignatoryLink -> IO Mail
mailInvitationToView' templates ctx document@Document{documenttitle} signaturelink = do
  let Just authorsiglink = getAuthorSigLink document
      authorname = signatoryname  $ signatorydetails authorsiglink
  title <- renderTemplate templates "mailInvitationToViewTitle" [("documenttitle", BS.toString documenttitle),
                                                                 ("creatorname",   BS.toString authorname)]
  content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document (Just signaturelink)
  return $ emptyMail {title = BS.fromString title, content = BS.fromString content}
    
mailDocumentClosed :: KontrakcjaTemplates -> Context -> Document -> IO Mail
mailDocumentClosed templates (Context {ctxhostpart}) document@Document{documenttitle} = 
   do
     title <- renderTemplate templates "mailDocumentClosedTitle" [("documenttitle",BS.toString  documenttitle )] 
     partylist <- renderListTemplate templates $  map (BS.toString . personname') $ partyList document
     content <- wrapHTML templates =<< (renderTemplateForProcess templates document processmailclosedcontent $ do
        field "documenttitle" $ BS.toString documenttitle 
        field "partylist" $ partylist
        field "ctxhostpart" $ ctxhostpart)
     return $ emptyMail {title = BS.fromString title, content = BS.fromString content}


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

mailCancelDocumentByAuthorContent :: KontrakcjaTemplates 
                                  -> Bool 
                                  -> (Maybe BS.ByteString)
                                  -> Context
                                  -> Document
                                  -> IO String
mailCancelDocumentByAuthorContent templates forMail customMessage ctx document = 
        let creatorname = BS.toString $ personname $ fromJust $ getAuthorSigLink document
            footer = if forMail
                     then if isNothing customMessage
                          then renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart", ctxhostpart ctx)] 
                          else renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                     else do
                       this <- renderTemplate templates "poweredBySkrivaPaPara" [("ctxhostpart", ctxhostpart ctx)] 
                       with <- renderTemplate templates "customFooter" [("creatorname", creatorname)] 
                       replaceOnEdit' templates this with
            header = case customMessage of 
              Just c -> return $ BS.toString c
              Nothing -> renderTemplateForProcess templates document processmailcancelbyauthorstandardheader $ do
                field "partylist" $ map (BS.toString . personname') $ partyList document
        in 
          do
            header' <- header
            editableHeader <- makeEditable' templates "customtext" header' 
            footer' <- footer
            renderTemplateForProcess templates document processmailcancelbyauthorcontent $ do
                field "header" editableHeader
                field "footer" footer'
                field "creatorname" creatorname
                field "documenttitle" $ BS.toString $ documenttitle document

mailCancelDocumentByAuthor :: KontrakcjaTemplates 
                           -> Maybe BS.ByteString 
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> IO Mail
mailCancelDocumentByAuthor templates customMessage ctx document@Document{documenttitle} signlink = 
       do
        title <- renderTemplate templates "mailCancelDocumentByAuthorTitle" [("documenttitle", BS.toString documenttitle)] 
        content <- wrapHTML templates =<< mailCancelDocumentByAuthorContent templates True customMessage ctx document
        return $ emptyMail { title = BS.fromString title
                           , to = [MailAddress { fullname = signatoryname $ signatorydetails signlink
                                              , email = signatoryemail $ signatorydetails $ signlink
                                              }]
                           , content = BS.fromString content
                           }

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
