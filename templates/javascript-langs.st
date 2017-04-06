####
#### This module puts in global context object 'localization'
#### If some script requires some string to be shown to the user
#### it just uses this module
####
javascriptLocalisation =
  window.localization =
        {      code : "$code$"
             , reloadPage : "$_reloadPage()$"
             , signviewDrawSignatureHere: "$_drawSignatureHere()$"
             , signviewMessage: "$_signviewMessage()$"
             , signviewHide: "$_signviewHide()$"
             , ssnInfoText: "$_SsnInfoText()$"
             , emailInfoText: "$_EmailInfoText()$"
             , nameInfoText: "$_NameInfoText()$"
             , phoneInfoText: "$_PhoneInfoText()$"
             , zoomOutMore: "$_ZoomOutMore()$"
             , zoomOut1: "$_ZoomOut1()$"
             , zoomOut2: "$_ZoomOut2()$"
             , signviewConfirmation: "$_contractSignConfirmation()$"
             , signviewConfirmationSignaturesPlaced: "$_contractSignConfirmationSignaturePlaced()$"
             , feedbackQ7Subtitle: "$_FeedbackQ7Subtitle()$"
             , feedbackQ7Title: "$_FeedbackQ7Title()$"
             , feedbackQ6Subtitle: "$_FeedbackQ6Subtitle()$"
             , feedbackQ6Title: "$_FeedbackQ6Title()$"
             , feedbackYesLink: "$_FeedbackYesLink()$"
             , feedbackQ5Subtitle: "$_FeedbackQ5Subtitle()$"
             , feedbackQ5Title: "$_FeedbackQ5Title()$"
             , feedbackPhoneNumber: "$_FeedbackPhoneNumber()$"
             , feedbackYesPlease: "$_FeedbackYesPlease()$"
             , feedbackNo: "$_FeedbackNo()$"
             , feedbackQ4Subtitle: "$_FeedbackQ4Subtitle()$"
             , feedbackQ4Title: "$_FeedbackQ4Title()$"
             , feedbackOften: "$_FeedbackOften()$"
             , feedbackSometimes: "$_FeedbackSometimes()$"
             , feedbackNever: "$_FeedbackNever()$"
             , feedbackQ3Subtitle: "$_FeedbackQ3Subtitle()$"
             , feedbackQ3Title: "$_FeedbackQ3Title()$"
             , feedbackFeedback: "$_FeedbackFeedback()$"
             , feedbackSend: "$_FeedbackSend()$"
             , feedbackSkip: "$_FeedbackSkip()$"
             , feedbackQ2Subtitle: "$_FeedbackQ2Subtitle()$"
             , feedbackQ2Title: "$_FeedbackQ2Title()$"
             , feedbackGood: "$_FeedbackGood()$"
             , feedbackBad: "$_FeedbackBad()$"
             , feedbackQ1: "$_FeedbackQ1()$"
             , signviewPdfOrPhoto: "$_pdfOrPhoto()$"
             , identifyDocument: "$_IdentifyDocument()$"
             , identifyBankId: "$_IdentifyBankId()$"
             , identifyBankIdNo: "$_IdentifyBankIdNO()$"
             , identifyBankIdNoMobile: "$_IdentifyBankIdNOMobile()$"
             , identifyBankIdError: {
               failed: "$_IdentifyBankIdNOFailed()$"
               , canceled: "$_IdentifyBankIdNOCanceled()$"
               , useragent: "$_IdentifyBankIdNOUserAgent()$"
               , auth: "$_IdentifyBankIdNOAuth()$"
               , blocked: "$_IdentifyBankIdNOBlocked()$"
               , revoked: "$_IdentifyBankIdNORevoked()$"
               , expired: "$_IdentifyBankIdNOExpired()$"
               , mobile: "$_IdentifyBankIdNOMobileError()$"
             }
             , identifyDKNemIdError: {
               failed: "$_IdentifyDKNemIdFailed()$"
               , canceled: "$_IdentifyDKNemIdCanceled()$"
               , useragent: "$_IdentifyDKNemIdUserAgent()$"
               , auth: "$_IdentifyDKNemIdAuth()$"
               , blocked: "$_IdentifyDKNemIdBlocked()$"
               , revoked: "$_IdentifyDKNemIdRevoked()$"
               , expired: "$_IdentifyDKNemIdExpired()$"
             }
             , identifyPhoneError: "$_IdentifyPhoneError()$"
             , identifyPhoneNumber: "$_IdentifyPhoneNumber()$"
             , identifyPhoneNumber8: "$_IdentifyPhoneNumber8()$"
             , identifyDateOfBirth: "$_IdentifyDateOfBirth()$"
             , openBankId: "$_OpenBankId()$"
             , yourIdNumber: "$_YourIdNumber()$"
             , idNumber: "$_IdNumber()$"
             , verifyIdentityWithName : "$_VerifyIdentityWithName()$"
             , verifyIdentityWithoutName : "$_VerifyIdentityWithoutName()$"
             , problemContactingServer : "$_problemContactingServer()$"
             , esigningpoweredbyscrive : "$PoweredByScrive()$"
             , sessionTimedoutInSignview : "$_sessionTimedoutInSignview()$"
             , signviewSigningFailed : "$_signviewSigningFailed()$"
             , signup : "$_signUp()$"
             , more : "$_more()$"
             , signviewAttachmentUploadedInOtherWindow : "$_signviewAttachmentUploadedInOtherWindow()$"
             , personalName : "$_fullName()$"
             , fstname : "$_firstname()$"
             , sndname : "$_lastname()$"
             , email : "$_email()$"
             , phone : "$_phone()$"
             , company : "$_company()$"
             , personalNumber : "$_personnumber()$"
             , companyNumber : "$_orgnumber()$"
             , next : "$_next()$"
             , cannotWithdrawAlreadySignedDocument : "$_cannotWithdrawAlreadySignedDocument()$"
             , statusToolTip: {
                   draft: "$_tooltipdraft()$"
                 , cancelled : "$_tooltipcancelled()$"
                 , sent : "$_tooltipsent()$"
                 , delivered : "$_tooltipdelivered()$"
                 , read : "$_tooltipread()$"
                 , opened : "$_tooltipopened()$"
                 , signed : "$_tooltipsigned()$"
                 , rejected :  "$_tooltiprejected()$"
                 , deliveryproblem : "$_tooltipdeliveryproblem()$"
                 , timeouted       : "$_tooltiptimedout()$"
                 , template : "$_tooltiptemplate()$"
                 , problem           : "$_tooltiperror()$"
             }
             , listand : "$_listand()$"
             , you : "$_you()$"
             , searchBoxButtonText : "$_searchBoxButtonText()$"
             , reviewAttachment: "$_ReviewAttachment()$"
             , reviewPDF: "$_ReviewPDF()$"
             , download : "$_Download()$"
             , deletePDF: "$_DeletePDF()$"
             , accept : "$_Accept()$"
             , send : "$_send()$"
             , csvFilePersons : "$_csvFilePersons()$"
             , reminder : {
                         send : "$_authorviewSignatoriesSendReminderButton()$"
                       , formHead : "$_reminderFormHead()$"
                       , formSend : "$_reminderFormSendReminder()$"
                       , formOwnMessage : "$_reminderFormFooterOwnMessage()$"
                       , mobileQuestion : "$_reminderByMobileQuestion()$"
                       , mobileQuestionAlreadySigned : "$_reminderByMobileQuestionAlreadySigned()$"
                       , emailMobileQuestion : "$_reminderByEmailMobileQuestion()$"
                    }
             , signByAuthor : {
                          modalTitle : "$_dialogConfirmSignByAuthorHead()$"

               }
             , reject : {
                         send : "$_pageDocumentForSignContentRejectFooterReject()$"
               }
             , signatoryMessage : {
                         rejected : "$_signatoryMessageIfSignatoryRejected()$"
                       , seen : "$_signatoryMessageIfSignatorySeendate()$"
                       , read : "$_signatoryMessageIfSignatoryReaddate()$"
                       , delivered : "$_signatoryMessageIfSignatoryDeliveredEmail()$"
                       , signed : "$_signatoryMessageIfSigned()$"
                       , other : "$_signatoryMessageIfElse()$"
                       , waiting : "$_signatoryMessageWaiting()$"
                       , deliveryproblem : "$_signatoryMessageDeliveryProblem()$"
                    }
            , authorAttachmentBoxHeader : "$_signViewNotsignedbymeAttachmentBoxHead()$"
            , evidenceAttachmentBoxHeader : "$_evidenceAttachmentBoxHeader()$"
            , signatoryAttachmentUploadButton: "$_signViewSigAttachmentBoxChooseFile()$"
            , signatoryAttachmentAttachedBy: "$_attachmentAttachedBy()$"
            , months : [ "$_january()$",
                         "$_february()$",
                         "$_march()$",
                         "$_april()$",
                         "$_may()$",
                         "$_june()$",
                         "$_july()$",
                         "$_august()$",
                         "$_september()$",
                         "$_october()$",
                         "$_november()$",
                         "$_december()$"
                       ]

            , fileTooLarge: "$_fileTooLarge()$"
            , couldNotUpload: "$_couldNotUpload()$"
            , couldNotUploadOnlyPDF: "$_couldNotUploadOnlyPDF()$"
            , flashDocumentRestarted : "$_flashMessageContractRestarted()$"
            , prolongmodal : {
                 title : "$_prolongModalTitle()$"
               , text  : "$_prolongModalText()$"
               , days  : "$_prolongModalDays()$"
               , button : "$_prolongModalButton()$"
              }
            , autoreminders : {
                 days : "$_autoreminderDays()$"
               , dueDateIn  : "$_autoreminderDueDateIn()$"
               , daysToRemind  : "$_autoreminderDaysToRemind()$"
               , automaticRemindersTitle : "$_autoreminderTitle()$"
               , changeAutoReminderTitle : "$_autoreminderChangeTitle()$"
               , changeAutoreminderButton : "$_autoreminderChangeButton()$"
               , setAutoReminderTitle : "$_autoreminderSetTitle()$"
               , setAutoreminderButton : "$_autoreminderSetButton()$"
               , removeAutoreminderButton : "$_autoreminderRemoveButton()$"
               , setDate : "$_autoreminderSetDate()$"
               , changeDate : "$_autoreminderChangeDate()$"
               , changeAutoreminderDescription : "$_autoreminderChangeDescription()$"
               , willBeSentOn : "$_autoreminderWillBeSentOn()$"
               , sendReminderIn : "$_autoreminderSendIn()$"
              }

            , process : {
               signModalTitle: "$_contractSignatorySignModalTitle()$",
               sendbuttontext: "$_contractsendbuttontext()$",
               startsigningbuttontext: "$_contractstartsigningbuttontext()$",
               restartbuttontext: "$_contractrestartbuttontext()$",
               prolongbuttontext: "$_contractprolongbuttontext()$",
               cancelbuttontext: "$_contractcancelbuttontext()$",
               cancelmodaltitle: "$_contractcancelmodaltitle()$",
               cancelmodaltext: "$_contractcancelmodaltext()$",
               cancelmodaltextwithsignatures: "$_contractcancelmodaltextwithsignatures()$",
               rejectbuttontext: "$_contractrejectbuttontext()$",
               rejectbuttontextwithoutreason: "$_contractrejectbuttontextwithoutreason()$",
               signbuttontext: "$_contractsignbuttontext()$",
               signbuttontextfromsignaturedrawing: "$_contractsignbuttontextfromsignaturedrawing()$",
               signatorycancelmodaltitle: "$_contractsignatorycancelmodaltitle()$",
               signatorycancelmodaltext: "$_contractsignatorycancelmodaltext()$",
               signatorycancelmodaltextwithoutreason: "$_contractsignatorycancelmodaltextwithoutreason()$",
               signatorycancelmodalplaceholder: "$_contractsignatorycancelmodalplaceholder()$",
               remindagainbuttontext: "$_contractremindagainbuttontext()$",
               authorname: "$_contractauthorname()$",
               authorsignatoryname: "$_contractauthorsignatoryname()$",
               signatoryname: "$_contractsignatoryname()$",
               nonsignatoryname: "$_contractnonsignatoryname()$"
              }
            , filterByTime: {
                filterForm: "$_fromFilter()$",
                filterTo: "$_toFilter()$"
            }
            , filterByStatus: {
                showAnyStatus: "$_showAnyStatusClass()$",
                showDraft: "$_tablestatusesDraft()$",
                showCancelled: "$_tablestatusesCancelled()$",
                showSent: "$_filterBySent()$",
                showSigned: "$_tablestatusesSigned()$"
            }
           , filterByAuthor : {
                showAnyAuthor : "$_anyAuthor()$"
            }
           , uploadButton: "$_signStepsBodyUploadBoxStepButtonChooseFile()$"
           , ok : "$_ok()$"
           , selectFiles: "$_selectFilesYouWantToUpload()$"
           , authorattachments: {
                  selectAttachments: "$_updateAttachmentsDialogHead()$"
                , selectAttachment : "$_updateAttachmentsDialogUpLoadChooseAttachment()$"
                , selectFile: "$_updateAttachmentsDialogUpLoadChooseFile()$"
                , invalidAttachments : "$_invalidAttachments()$"
                , tooLargeAttachment : "$_tooLargeAttachment()$"
                , tooLargeAttachments : "$_tooLargeAttachments()$"
                , required : "$_authorAttachmentRequired()$"
                , optional : "$_authorAttachmentOptional()$"
                , nameOfAttachment : "$_authorAttachmentName()$"
                , typeOfAttachment :  "$_authorAttachmentType()$"
                , back : "$_authorAttachmentBack()$"
                , add : "$_authorAttachmentAdd()$"
                , mergedQuestion : "$_authorAttachmentMergedQuestion()$"
                , merged : "$_authorAttachmentMerged()$"
                , notMerged : "$_authorAttachmentNotMerged()$"
            }
          , signatoryAttachments : {
                addAttachment:      "$_pageDocumentForAuthorHelpersLocalDialogsAddAttachment()$"
              , description:        "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentdesc()$"
              , attachment:         "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentname()$"
              , from :              "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentsigs()$"
              , requestAttachments : "$_pageDocumentForAuthorHelpersLocalDialogsHead()$"
              , defineRequests :     "$_pageDocumentForAuthorHelpersLocalDialogsDefineRequests()$"
              , uniqueAttachmentNamesError : "$_pageDocumentForAuthorHelpersLocalDialogsUniqueAttachmentNamesError()$"
              , errorFlashMessage : "$_pageDocumentForAuthorHelpersLocalDialogsErrorFlashMessage()$"
            }
          , save : "$_save()$"
          , languages : {
                en : "$_English()$"
              , sv : "$_Swedish()$"
              , de : "$_German()$"
              , fr : "$_French()$"
              , it : "$_Italian()$"
              , es : "$_Spanish()$"
              , pt : "$_Portuguese()$"
              , nl : "$_Dutch()$"
              , da : "$_Danish()$"
              , no : "$_Norwegian()$"
              , el : "$_Greek()$"
              , fi : "$_Finnish()$"
              , is : "$_Icelandic()$"
              , et : "$_Estonian()$"
              , lv : "$_Latvian()$"
              , lt : "$_Lithuanian()$"
              , svInSv :"$SwedishInSwedish()$"
              , enInEn : "$EnglishInEnglish()$"
              , deInDe : "$GermanInGerman()$"
              , frInFr : "$FrenchInFrench()$"
              , itInIt : "$ItalianInItalian()$"
              , esInEs : "$SpanishInSpanish()$"
              , ptInPt : "$PortugueseInPortuguese()$"
              , nlInNl : "$DutchInDutch()$"
              , daInDa : "$DanishInDanish()$"
              , noInNo : "$NorwegianInNorwegian()$"
              , elInEl : "$GreekInGreek()$"
              , fiInFi : "$FinnishInFinish()$"
              , isInIs : "$IcelandicInIcelandic()$"
              , etInEt : "$EstonianInEstonian()$"
              , lvInLv : "$LatvianInLatvian()$"
              , ltInLt : "$LithuanianInLithuanian()$"
            }
         , csv : {
                title : "$_dialogCsvupload1Head()$"
              , subtitle : "$_dialogCsvupload1Subtitle()$"
              , selectFile :  "$_dialogCsvupload2CSVMultiChooseFile()$"
              , formatError : "$_dialogCsvuploadFormatError()$"
              , atLeast3Columns : "$_dialogCsvuploadAtLeast3Columns()$"
              , atLeast1Party : "$_dialogCsvuploadAtLeast1Party()$"
            }
         , signature : {
               placeFor : "$_signaturePlaceFor()$"
             , clickToDraw : "$_clickToDraw()$"

           }
          , toStart: {
               docSent: "$_toStartDocSent()$",
               sendForSigning: "$_sendForSigning()$"
               , backFromSigningPage: "$_afterErrorGoBack()$"
               , someFieldsNotFilledInCorrectly: "$_flashMessageSomeFieldsAreIncorrectlyFilledIn()$"
          }
         , phonePlaceholder: "$_phonePlaceholder()$"
         , designview : {
              dndDisabled: "$_designViewDNDDisabled()$",
              editInvitationMessagePlaceholder: "$_editInvitationMessagePlaceholder()$",
              editConfirmationMessagePlaceholder: "$_editConfirmationMessagePlaceholder()$",
              validation : {
                      notReadyField : "$_setAllFields()$"
                    , requestAttachmentFlashMessage : "$_designViewAttachmentRequestedFlashMessage()$"
                }
             , signviewsettings : {
                  showheader: "$_signviewSettingsShowHeader()$"
                , showpdfdownload: "$_signviewSettingsShowPDFDownload()$"
                , showrejectoption: "$_signviewSettingsShowRejectOption()$"
                , allowrejectreason: "$_signviewSettingsAllowRejectReason()$"
                , showfooter: "$_signviewSettingsShowFooter()$"
                , title: "$_signviewSettingsTitle()$"
                , button: "$_signviewSettingsButton()$"
               }
             , sign : "$_sign()$"
             , textFields : {
                      obligatory: "$_textFieldObligatory()$"
                    , forThis : "$_textFieldFor()$"
                    , done : "$_textFieldDone()$"
                    , remove : "$_textFieldRemove()$"
             }
             , checkboxes : {
                      prechecked : "$_checkboxPrechecked()$"
                    , obligatory: "$_checkboxObligatory()$"
             }
             , cantSignModal : {
                 title : "$_designviewCantSignModalTitle()$"
               , info1 : "$_designviewCantSignModalInfo1()$"
               , info2 : "$_designviewCantSignModalInfo2()$"
               , li1 : "$_designviewCantSignModalLi1()$"
               , li2 : "$_designviewCantSignModalLi2()$"
               , li3 : "$_designviewCantSignModalLi3()$"
               , li4 : "$_designviewCantSignModalLi4()$"
             }
             , editParticipants: "$_designViewEditParticipants()$"
             , editDocument: "$_designViewEditDocument()$"
             , newDocumentTitle: "$_newDocumentTitle()$"
             , optionalField: "$_designViewOptionalField()$"
             , anchor: "$_designViewAnchor()$"
             , addAnchor: "$_designViewAddAnchor()$"
             , anchorAddPhrase: "$_designViewAddPhrase()$"
             , anchorIndex: "$_designViewAnchorIndex()$"
             , anchorSearchPhrase: "$_designViewAnchorSearchPhrase()$"
             , editSigningProcess: "$_designViewEditSigningProcess()$"
             , startSigning: "$_designViewStartSigning()$"
             , addField: "$_designViewAddField()$"
             , editField: "$_designViewEditField()$"
             , whatField: "$_designViewWhatField()$"
             , customField: "$_designViewCustomField()$"
             , customFieldLabel: "$_designViewCustomFieldLabel()$"
             , fieldName: "$_designViewFieldName()$"
             , addParty: "$_designViewAddParty()$"
             , moreSettings: "$_designViewMoreSettings()$"
             , selectField: "$_designViewSelectField()$"
             , optionalMandatory: "$_designViewOptionalMandatory()$"
             , addParties : {
                 close : "$_designViewAddPartiesClose()$"
               , invitationOrder : "$_designViewAddPartiesInvitationOrder()$"
               , invitation : "$_designViewAddPartiesInvitation()$"
               , invitationEmail : "$_designViewAddPartiesInvitationEmail()$"
               , invitationInPerson : "$_designViewAddPartiesInvitationInPerson()$"
               , invitationNone : "$_designViewAddPartiesInvitationNone()$"
               , invitationSMS : "$_designViewAddPartiesInvitationSMS()$"
               , invitationEmailSMS : "$_designViewAddPartiesInvitationEmailSMS()$"
               , invitationAPI : "$_designViewAddPartiesInvitationAPI()$"
               , role : "$_designViewAddPartiesRole()$"
               , roleSignatory : "$_designViewAddPartiesRoleSignatory()$"
               , roleViewer : "$_designViewAddPartiesRoleViewer()$"
               , authenticationToView : "$_designViewAddPartiesAuthenticationToView()$"
               , authenticationToViewStandard : "$_designViewAddPartiesAuthenticationToViewStandard()$"
               , authenticationToViewSEBankID : "$_designViewAddPartiesAuthenticationtoViewSEBankID()$"
               , authenticationToViewNOBankID : "$_designViewAddPartiesAuthenticationtoViewNOBankID()$"
               , authenticationToViewDKNemID : "$_designViewAddPartiesAuthenticationtoViewDKNemID()$"
               , authenticationToSign : "$_designViewAddPartiesAuthentication()$"
               , authenticationToSignStandard : "$_designViewAddPartiesAuthenticationStandard()$"
               , authenticationToSignSEBankID : "$_designViewAddPartiesAuthenticationSEBankID()$"
               , authenticationToSignSMSPin : "$_designViewAddPartiesAuthenticationSMSPin()$"
               , confirmation : "$_designViewAddPartiesConfirmation()$"
               , confirmationEmail : "$_designViewAddPartiesConfirmationEmail()$"
               , confirmationSMS : "$_designViewAddPartiesConfirmationSMS()$"
               , confirmationEmailSMS : "$_designViewAddPartiesConfirmationEmailSMS()$"
               , confirmationNone : "$_designViewAddPartiesConfirmationNone()$"
             }
             , addMultisend: "$_designViewAddMultisend()$"
             , viewCSV: "$_designViewViewCSV()$"
             , participantsInFile: "$_participantsIncludedInFile()$"
             , fullName: "$_fullName()$"
             , mandatoryForRecipient: "$_designViewMandatoryForRecipient()$"
             , mandatoryForSender: "$_designViewMandatoryForSender()$"
             , documentName: "$_designViewDocumentName()$"
             , recipientsLanguage: "$_designViewRecipientsLanguage()$"
             , signingDeadline: "$_designViewSigningDeadline()$"
             , addRemove: "$_designViewAddRemove()$"
             , request: "$_designViewRequest()$"
             , editInvitation: "$_designViewEditInvitation()$"
             , editConfirmation: "$_designViewEditConfirmation()$"
             , signatureBox: "$_designViewSignatureBox()$"
             , freeTextBox: "$_designViewFreeTextBox()$"
             , checkbox: "$_checkBoxFieldName()$"
             , signature: "$_signatureFieldName()$"
             , removeThisDocument: "$_designViewRemoveThisDocument()$"
             , attached : "$_designViewAttached()$"
             , attachmentRequestedFrom : "$_designViewAttachmentRequestedFrom()$"
             , draggablehelp1 : "$_designviewdraggablehelp1()$"
             , draggablehelp2 : "$_designviewdraggablehelp2()$"
             , days :"$_designviewdays()$"
             , saveAsDraftButton : "$_designViewSaveAsDraftButton()$"
             , saveAsTemplateButton : "$_designViewSaveAsTemplateButton()$"
             , saveDraftButton : "$_designViewSaveDraftButton()$"
             , saveTemplateButton : "$_designViewSaveTemplateButton()$"
             , saved : {
                 saveDraft       : "$_designViewSaveDraftFlashMessage()$"
               , saveTemplate    : "$_designViewSaveTemplateFlashMessage()$"
               , savedAsDraft    : "$_designViewSavedAsDraftFlashMessage()$"
               , savedAsTemplate : "$_designViewSavedAsTemplateFlashMessage()$"
             }
             , fromCSV : "$_fromCSV()$"
             , numPartOfNum : "$_numPartOfNum()$"
             , preparingDocument : "$_preparingDocument()$"
             , fieldWithSameNameExists : "$_fieldWithSameNameExists()$"
             , customMessage : {
                 invitation   : "$_designViewCustomMessageInvitation()$"
               , confirmation : "$_designViewCustomMessageConfirmation()$"
               , preview      : "$_designViewCustomMessagePreview()$"
             }
             , signModalContentAuthorOnly : "<p>$_designViewSignModalContentAuthorOnly()$</p>"
             , signModalContentAuthorOnlyWithName : "<p>$_designViewSignModalContentAuthorOnlyWithName()$</p>"
             , signModalContent : "<p>$_designViewSignModalContent()$</p>"
             , signModalContentWithName : "<p>$_designViewSignModalContentWithName()$</p>"
             , startSigningModalTextWithPad : "$_designViewStartSigningModalTextWithPad()$"
             , startSigningModalText : "$_designViewSigningModalText()$"
             , startSigningModalTextWithSendout : "$_designViewStartSigningModalTextWithSendout()$"
             , lastViewerOnlyGetsConfirmation : "$_designViewLastViewerOnlyGetsConfirmation()$"
             , viewerCantHaveAuthorisation : "$_designViewViewerCantHaveAuthorisation()$"
             , removePage : {
                 modalTitle : "$_designViewRemovePageModalTitle()$"
               , modalBody1 : "$_designViewRemovePageModalBody1()$"
               , modalBody2 : "$_designViewRemovePageModalBody2()$"
               , modalRemoveButton : "$_designViewRemovePageModalButton()$"
             }
             , dropToUpload: "$_designViewDropToUpload()$"
         }
         , validation : {
                required: "$_nameRequired()$"
              , toolong: "$_nameTooLong()$"
              , invalidnamechars: "$_nameInvalidChars()$"
              , companyNumberInvalidChars: "$_companyNumberInvalidChars()$"
              , companyNumberTooLong: "$_companyNumberTooLong()$"
              , passwordExceedsMaxLength: "$_PasswordValidationMessageInputExceedsMaxLength()$"
              , passwordLessThanMinLength: "$_PasswordValidationMessageInputLessThanMinLength()$"
              , passwordNeedsLetterAndDigit: "$_PasswordValidationMessageNeedsLetterAndDigit()$"
              , passwordsDontMatch: "$_PasswordValidationMessagePasswordsDontMatch()$"
              , mustAcceptTOS: "$_flashMessageMustAcceptTOS()$"
              , wrongEmail : "$_emptyOrNotValidEmail()$"
              , passwordOldPasswordNotValid : "$_flashMessageBadOldPassword()$"
         }
         , pad : {
                cleanImage : "$_padCleanImage()$"
             ,  notNamedParty : "$_notNamedParty()$"
             ,  changePartyTo : "$_changePartyTo()$"
             ,  giveForSigningThisDevice : "$_padGiveForSigningThisDevice()$"
             ,  backToList : "$_padBackToList()$"
             ,  backToDocument : "$_padBackToDocument()$"
        }
         , changeEmail : "$_changeEmail()$"
         , changeEmailModal: {
             title: "$_changeEmailModalTitle()$"
           , label: "$_changeEmailModalLabel()$"
           , acceptButton: "$_changeEmailModalAcceptButton()$"
           , invalidEmailFlash: "$_changeEmailModalAcceptInvalidEmailFlash()$"
           , placeholder: "$_changeEmailModalPlaceholder()$"
         }
         , changeMobileModal: {
             title: "$_changeMobileModalTitle()$"
           , label: "$_changeMobileModalLabel()$"
           , acceptButton: "$_changeMobileModalAcceptButton()$"
           , invalidMobileFlash: "$_changeMobileModalAcceptInvalidMobileFlash()$"
           , placeholder: "$_changeMobileModalPlaceholder()$"
         }
         , changePhone : "$_changePhone()$"
         , newPasswordModal: {
             modalNewPasswordViewNewPassword: "$_modalNewPasswordViewNewPassword()$"
           , modalNewPasswordViewRepeatPassword: "$_modalNewPasswordViewRepeatPassword()$"
           , modalNewPasswordViewFooterSave: "$_modalNewPasswordViewFooterSave()$"
           , flashMessagePasswordsDontMatch: "$_flashMessagePasswordsDontMatch()$"
           , flashMessagePasswordChangeLinkNotValid: "$_flashMessagePasswordChangeLinkNotValid()$"
           , flashMessageUserPasswordChanged : "$_flashMessageUserPasswordChanged()$"
         }
         , signupModal: {
             modalAccountSetupFooter: "$_modalAccountSetupFooter()$"
           , alreadyHaveAnAccount : "$_alreadyHaveAccount()$"
         }
         , accountSetupModal: {
             modalAccountSetupTOS: "$_modalAccountSetupTOS()$"
           , modalAccountSetupChoosePassword: "$_modalAccountSetupChoosePassword()$"
           , modalAccountSetupRepeatPassword: "$_modalAccountSetupRepeatPassword()$"
           , modalAccountSetupPhone: "$_modalAccountSetupPhone()$"
           , modalAccountSetupPosition : "$_modalAccountSetupPosition()$"
           , modalAccountSetupCompany : "$_modalAccountSetupCompany()$"
           , modalAccountSetupMissingName : "$_modalAccountSetupMissingName()$"
           , modalAccountPasswordRequirements : "$_modalAccountPasswordRequirements()$"
           , flashMessageUserAlreadyActivated : "$_flashMessageUserAlreadyActivated()$"
           , flashMessageUserActivated : "$_flashMessageUserActivated()$"
         }
         , cancel : "$_cancel()$"
         , authorview : {
               signatoriesTitle : "$_authorviewSignatoriesTitle()$"
             , requestedAttachments : "$_RequestedAttachments()$"
             , requestedFrom : "$_authorviewRequestedFrom()$"
             , uploadedBy    : "$_authorviewUploadedBy()$"
             , signNow : "$_authorviewSignNow()$"
             , signedAndClosed : "$_authorviewSignedAndClosed()$"
             , closed : "$_authorviewClosed()$"
             , signedNotClosed : "$_authorviewSignedNotClosed()$"
             , canceled : "$_authorviewCanceled()$"
             , rejected : "$_authorviewRejected()$"
             , timeouted : "$_authorviewTimeouted()$"
             , pending  : "$_authorviewPending()$"
             , goToSignView : "$_authorviewGoToSignview()$"
             , downloadPdf : "$_DownloadPDF()$"
         }
         , history : {
               time  : "$_historyTime()$"
             , party : "$_historyParty()$"
             , description : "$_historyDescription()$"
             , expand : "$_historyExpand()$"
             , hide : "$_historyHide()$"
           }
         , docsignview : {
               followTheArrow: "$_followTheArrow()$"
             , followTheArrowWithUserName: "$_followTheArrowWithUserName()$"
             , reviewDocument: "$_reviewDocument()$"
             , signedNotClosed: "$_signedNotClosed()$"
             , signedAndClosed: "$_signedAndClosed()$"
             , unavailableForSign: "$_unavailableForSign()$"
             , dueDate: "$_dueDate()$"
             , companyNumberLabel: "$_companyNumberLabel()$"
             , personalNumberLabel: "$_personalNumberLabel()$"
             , authorAttachmentsUnderstoodContent : "$_authorAttachmentsUnderstoodContent()$"
             , signatoriesTitle: "$_signViewSignatoriesTitle()$"
             , notEntered: "$_notEntered()$"
             , filladitionfieldslabel: "$_filladitionfieldslabel()$"
             , downloadDocumentButtonText: "$_downloadDocumentButtonText()$"
             , continueButtonText: "$_continueButtonText()$"
             , errorMessagePrefix: "$_errorMessagePrefix()$"
             , networkIssueErrorMessage: "$_networkIssueErrorMessage()$"
             , pinSigning : {
                  invalidPhone   :  "$_invalidPhone()$"
                , signWithSMSPin :  "$_signWithSMSPin()$"
                , noPinProvided   : "$_noPinProvided()$"
                , invalidPin      : "$_invalidPin()$"
                , enterSMSPin     : "$_enterSMSPin()$"
                , pinWillBeSentTo : "$_pinWillBeSentTo()$"
                , checkYourPhone : "$_checkYourPhone()$"

             }
             , controlsStatus : {
                  highlighting     : "$_controlsStatusHighlighting()$"
                , clearingHighlight: "$_controlsStatusClearingHighlight()$"
                , viewingPending   : "$_controlsStatusViewingPending()$"
                , viewingSigning   : "$_controlsStatusViewingSigning()$"
                , viewingClosed    : "$_controlsStatusViewingClosed()$"
                , viewingCanceled  : "$_controlsStatusViewingCanceled()$"
                , viewingRejected  : "$_controlsStatusViewingRejected()$"
                , viewingTimedout : "$_controlsStatusViewingTimedout()$"
                , authornamePlaceholder : "$_authorParty()$"
             }
             , clearHighlightingHere: "$_clearHighlightingHere()$"
             , eleg : {
                  bankid : {
                        faultModalTitle : "$_elegBankIDFaultModalTitle()$"
                      , startAppNow : "$_elegBankIDStartAppNow()$"
                      , signConfirmationTitle : "$_elegSignConfirmationTitle()$"
                      , signConfirmationText : "<p>$_elegSignConfirmationText()$</p>"
                      , signConfirmationTextNoName : "$_elegSignConfirmationTextNoName()$"
                      , rfa1 : "$_elegBankIDRFA1()$"
                      , rfa3 : "$_elegBankIDRFA3()$"
                      , rfa5 : "$_elegBankIDRFA5()$"
                      , rfa6 : "$_elegBankIDRFA6()$"
                      , rfa8 : "$_elegBankIDRFA8()$"
                      , rfa9 : "$_elegBankIDRFA9()$"
                      , rfa12 : "$_elegBankIDRFA12()$"
                      , rfa13 : "$_elegBankIDRFA13()$"
                      , rfa14 : "$_elegBankIDRFA14()$"
                      , rfa16 : "$_elegBankIDRFA16()$"
                      , rfa17 : "$_elegBankIDRFA17()$"
                      , rfa18 : "$_elegBankIDRFA18()$"
                      , accessDenied : "$_elegBankAccessDenied()$"
                      , invalidParametersCanChange : "$_elegBankInvalidParametersCanChange()$"
                      , invalidParametersCantChange : "$_elegBankInvalidParametersCantChange()$"
                  }
             }
        }
        , docview : {
            signatory : {
                editAuthenticationToViewMethod: "$_docviewSignatoryeditAuthenticationToViewMethod()$"
              , editAuthenticationToSignMethod: "$_docviewSignatoryeditAuthenticationToSignMethod()$"
              , showAPIDelivery: "$_docviewSignatoryShowAPIDelivery()$"
              , invitationOrder: "$_docviewSignatoryInvitationOrderLabel()$"
              , invitationMethod: "$_docviewSignatoryInvitationMethodLabel()$"
              , invitationEmail: "$_docviewSignatoryInvitationEmail()$"
              , invitationPad: "$_docviewSignatoryInvitationPad()$"
              , invitationSMS: "$_docviewSignatoryInvitationSMS()$"
              , invitationEmailSMS: "$_docviewSignatoryInvitationEmailSMS()$"
              , invitationAPI: "$_docviewSignatoryInvitationAPI()$"
              , invitationNone: "$_docviewSignatoryInvitationNone()$"
              , role: "$_docviewSignatoryRoleLabel()$"
              , roleSignatory: "$_docviewSignatoryRoleSignatory()$"
              , roleViewer: "$_docviewSignatoryRoleViewer()$"
              , authenticationToView: "$_docviewSignatoryAuthenticationToViewMethodLabel()$"
              , authenticationToViewStandard: "$_docviewSignatoryAuthenticationToViewStandard()$"
              , authenticationToViewSEBankID: "$_docviewSignatoryAuthenticationToViewSEBankID()$"
              , authenticationToViewNOBankID: "$_docviewSignatoryAuthenticationToViewNOBankID()$"
              , authenticationToViewDKNemID: "$_docviewSignatoryAuthenticationToViewDKNemID()$"
              , authenticationToSign: "$_docviewSignatoryAuthenticationToSignMethodLabel()$"
              , authenticationToSignStandard: "$_docviewSignatoryAuthenticationToSignStandard()$"
              , authenticationToSignSMSPin: "$_docviewSignatoryAuthenticationToSignSMSPin()$"
              , authenticationToSignSEBankID: "$_docviewSignatoryAuthenticationToSignSEBankID()$"
              , confirmation: "$_docviewSignatoryConfirmationMethodLabel()$"
              , confirmationEmail: "$_docviewSignatoryConfirmationEmail()$"
              , confirmationSMS: "$_docviewSignatoryConfirmationSMS()$"
              , confirmationEmailSMS: "$_docviewSignatoryConfirmationEmailSMS()$"
              , confirmationNone: "$_docviewSignatoryConfirmationNone()$"
            },
            showAPIDelivery : {
                title: "$_docviewShowAPIDeliveryModalTitle()$"
              , accept: "$_docviewShowAPIDeliveryModalAccept()$"
              , description: "$_docviewShowAPIDeliveryModalDescription()$"
            },
            changeAuthenticationToView : {
                title: "$_docviewChangeAuthenticationToViewModalTitle()$"
              , accept: "$_docviewChangeAuthenticationToViewModalAccept()$"
              , methodLabel: "$_docviewChangeAuthenticationToViewModalMethodLabel()$"
              , ssnSEBankIDLabel: "$_docviewChangeAuthenticationToViewModalSEBankIDSSNLabel()$"
              , ssnSEBankIDPlaceholder: "$_docviewChangeAuthenticationToViewModalSEBankIDPlaceholder()$"
              , ssnNOBankIDLabel: "$_docviewChangeAuthenticationToViewModalNOBankIDSSNLabel()$"
              , ssnNOBankIDPlaceholder: "$_docviewChangeAuthenticationToViewModalNOBankIDPlaceholder()$"
              , ssnDKNemIDLabel: "$_docviewChangeAuthenticationToViewModalDKNemIDSSNLabel()$"
              , ssnDKNemIDPlaceholder: "$_docviewChangeAuthenticationToViewModalDKNemIDPlaceholder()$"
              , phoneLabel: "$_docviewChangeAuthenticationToViewModalPhoneLabel()$"
              , flashMessageInvalidSESSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidSESSN()$"
              , flashMessageInvalidNOSSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidNOSSN()$"
              , flashMessageInvalidDKSSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidDKSSN()$"
              , flashMessageInvalidNOPhone: "$_docviewChangeAuthenticationToViewFlashMessageInvalidNOPhone()$"
              , flashMessageInvalidNOSSNAndPhone: "$_docviewChangeAuthenticationToViewFlashMessageInvalidNOSSNAndPhone()$"
              , errorFlashMessage: "$_docviewChangeAuthenticationToViewFlashMessageError()$"
            },
            changeAuthentication : {
                title: "$_docviewChangeAuthenticationModalTitle()$"
              , accept: "$_docviewChangeAuthenticationModalAccept()$"
              , methodLabel: "$_docviewChangeAuthenticationModalMethodLabel()$"
              , placeholderEID: "$_docviewChangeAuthenticationModalPlaceholderEID()$"
              , placeholderPhone: "$_docviewChangeAuthenticationModalPlaceholderPhone()$"
              , valueInfotext: "$_docviewChangeAuthenticationModalValueInfotext()$"
              , errorFlashMessage: "$_docviewChangeAuthenticationModalErrorFlashMessage()$"
              , errorPhone: "$_docviewChangeAuthenticationModalErrorPhone()$"
              , errorEID: "$_docviewChangeAuthenticationModalErrorEID()$"
            }
        }
        , verification : {
                success: "$_verificationSuccess()$"
             ,  failed: "$_verificationFailed()$"
             ,  failedMessage: "$_verificationFailedMessage()$"
             ,  error : "$_verificationError()$"
             ,  errorMessage : "$_verificationErrorMessage()$"
             ,  time: "$_verificationTime()$"
             ,  gateway : "$_verificationGateway()$"

          }
        , payments: {
             vat: "$_paymentsVat()$"
           , contact: "$_paymentPlanContact()$"
           , placeholder : "$_paymentPlanPlaceholder()$"
           , sendmsg : "$_paymentPlanSendMessage()$"
           , numberOfUsers: "$_paymentNumberOfUsers()$"
           , priceUnit: {
                  EUR: "$_paymentPriceUnitEUR()$"
                , SEK: "$_paymentPriceUnitSEK()$"
            }
           , allFeaturesIncluded: "$_paymentAllFeaturesIncluded()$"
           , payYearly: "$_paymentPayYearly()$"
           , payBiYearly: "$_paymentPayBiYearly()$"
           , plans: {
                    trial : {
                          name : "$_paymentPlanTrial()$"
                    }
                    , team : {
                          name : "$TeamPlanName()$"
                        , price : {
                              EUR: "$TeamPriceEURPerMonth()$"
                            , SEK: "$TeamPriceSEKPerMonth()$"
                        }
                        , priceWithDiscont : {
                              EUR: "$TeamPriceWithDiscountEURPerMonth()$"
                            , SEK: "$TeamPriceWithDiscountSEKPerMonth()$"
                        }
                        , branding: "$_paymentPlanTeamBranding()$"
                        , users: "$_paymentPlanTeamUsers()$"
                        , stores: "$_paymentPlanTeamStores()$"
                        , api: "$_paymentPlanTeamAPI()$"
                        , support: "$_paymentPlanTeamSupport()$"
                    }
                    , enterprise : {
                          name : "$EnterprisePlanName()$"
                        , price : {
                              EUR: "$EnterprisePriceEURPerMonth()$"
                            , SEK: "$EnterprisePriceSEKPerMonth()$"
                        }
                        , priceWithDiscont : {
                              EUR: "$EnterprisePriceWithDiscountEURPerMonth()$"
                            , SEK: "$EnterprisePriceWithDiscountSEKPerMonth()$"
                        }
                        , branding: "$_paymentPlanEnterpriseBranding()$"
                        , users: "$_paymentPlanEnterpriseUsers()$"
                        , stores: "$_paymentPlanEnterpriseStores()$"
                        , api: "$_paymentPlanEnterpriseAPI()$"
                        , support: "$_paymentPlanEnterpriseSupport()$"
                        , sla: "$_paymentPlanEnterpriseSLA()$"
                        , p2es: "$_paymentPlanEnterpriseP2ES()$"
                        , whiteLabel: "$_paymentPlanEnterpriseWhiteLabel()$"
                    }
                    , company : {
                          name : "$CompanyPlanName()$"
                        , price : {
                              EUR: "$CompanyPriceEURPerMonth()$"
                            , SEK: "$CompanyPriceSEKPerMonth()$"
                        }
                        , priceWithDiscont : {
                              EUR: "$CompanyPriceWithDiscountEURPerMonth()$"
                            , SEK: "$CompanyPriceWithDiscountSEKPerMonth()$"
                        }
                        , branding: "$_paymentPlanCompanyBranding()$"
                        , users: "$_paymentPlanCompanyUsers()$"
                        , stores: "$_paymentPlanCompanyStores()$"
                        , api: "$_paymentPlanCompanyAPI()$"
                        , support: "$_paymentPlanCompanySupport()$"
                        , sla: "$_paymentPlanCompanySLA()$"
                    }
                    , one : {
                          name : "$OnePlanName()$"
                        , price : {
                              EUR: "$OnePriceEURPerMonth()$"
                            , SEK: "$OnePriceSEKPerMonth()$"
                        }
                        , priceWithDiscont : {
                              EUR: "$OnePriceWithDiscountEURPerMonth()$"
                            , SEK: "$OnePriceWithDiscountSEKPerMonth()$"
                        }
                        , users: "$_paymentPlanOneUsers()$"
                        , support: "$_paymentPlanOneSupport()$"
                    }
           }
           , table: {
              currentplan : "$_paymentPlanCurrentPlan()$"
           }
           , contactSales : "$_paymentPlanAskViktor()$"
           , outside: {
                confirmAccountMaybeCreatedUser  : "$_confirmAccountMaybeCreatedUser()$"
           }
        }
      , documents : "$_docListTabsDocs()$"
      , templates : "$_docListTabsTemplates()$"
      , attachments : "$_docListTabsAttachemtns()$"
      , archive : {
                documents : {
                       name :  "$_docListTabsDocs()$"
                    ,  search : "$_SearchDoc()$"
                    ,  statusDescription : {
                              draft :     "$_tablestatusesDraft()$"
                            , cancelled : "$_tablestatusesCancelled()$"
                            , sent :      "$_tablestatusesSent()$"
                            , delivered:  "$_tablestatusesDelivered()$"
                            , read :      "$_tablestatusesEmailOpened()$"
                            , opened :    "$_tablestatusesViewedOnline()$"
                            , signed :    "$_tablestatusesSigned()$"
                       }
                    ,  sendreminder : {
                              action: "$_archiveSendReminder()$"
                            , bodymulti : "$_ContractReminderBodyMulti()$"
                            , bodysingle : "$_ContractReminderBodySingel()$"
                            , successMessage : "$_flashMessageBulkDocumentRemindsSent()$"
                            , errorMessage   : "$_flashMessageNoBulkDocumentRemindsSent()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                            , notAvailableMessage : "$_youCantSendRemindersToThisDocuments()$"
                        }
                    , cancel : {
                              action: "$_listcancelbuttontext()$"
                            , body : "$_listcancelbody()$"
                            , successMessage : "$_listcanceledflashmessagesuccess()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                            , notAvailableMessage : "$_youCantCancelThisDocuments()$"
                        }
                    ,  remove : {
                              action: "$_delete()$"
                            , body : "$_listLocalDialogsBody()$"
                            , successMessage : "$_flashMessageDocumentArchiveDone()$"
                            , invalidMessage : "$_flashMessageDocumentArchiveInvalid()$"
                        }
                    , csv : {
                              action : "$_archiveCSV()$"
                        }
                    , zip : {
                              action : "$_archiveZIP()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    , columns :{
                              status : "$_SortStatus()$"
                            , time   : "$_SortTime()$"
                            , sender : "$_SortSender()$"
                            , party  : "$_SortParty()$"
                            , title  : "$_SortDoc()$"

                       }
                    , idleRemark  : "$_documentsArchiveIdleRemark()$"
                    , idleRemark1 : "$_documentsArchiveIdleRemark1()$"
                }
            ,  templates : {
                       name :  "$_docListTabsTemplates()$"
                    ,  remove : {
                              action: "$_delete()$"
                            , body : "$_listLocalDialogsBody()$"
                            , successMessage : "$_flashMessageTemplateArchiveDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  share : {
                              action: "$_share()$"
                            , head : "$_listShareDialogHead()$"
                            , body : "$_listShareDialogBody()$"
                            , successMessage : "$_flashMessageTemplateShareDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  createnew : "$_createNewTemplate()$"
                    ,  search : "$_SearchDoc()$"
                    ,  columns :{
                              time       : "$_SortTime()$"
                            , template   : "$_SortTemplate()$"
                            , shared     : "$_shared()$"
                            , sharedBy   : "$_sharedBy()$"
                            , verificationMethod : "$_verificationMethod()$"
                       }
                }
            ,  attachments : {
                       name : "$_docListTabsAttachemtns()$"
                    ,  remove : {
                              action: "$_delete()$"
                            , body : "$_listLocalDialogsBody()$"
                            , successMessage : "$_flashMessageAttachmentArchiveDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  share : {
                              action: "$_pageAttachmentListShare()$"
                            , head : "$_listShareDialogHead()$"
                            , body : "$_listShareDialogBody()$"
                            , successMessage : "$_flashMessageAttachmentShareDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  createnew : {
                              action: "$_pageAttachmentListNewAttachment()$"
                       }
                    ,  search : "$_pageAttachmentListSearchInfo()$"
                    ,  columns :{
                              time       : "$_SortTime()$"
                            , shared      : "$_shared()$"
                            , attachment   : "$_SortAttachment()$"

                       }
                }
            ,  trash: {
                       name :  "$_docListTabsTrash()$"
                    ,  restore : {
                              action: "$_restore()$"
                            , head : "$_listRestoreDialogHead()$"
                            , body : "$_listRestoreDialogBody()$"
                            , successMessage : "$_flashMessageRubbishRestoreDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  remove : {
                              action: "$_delete()$"
                            , body : "$_listLocalDialogsBody()$"
                            , cannotUndo: "$_thisActionCannotBeUndone()$"
                            , successMessage : "$_flashMessageDocumentArchiveDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    , subheadline : "$_trashSubheadline()$"
                    ,  search : "$_SearchDoc()$"
               }
        }
      , padlist : {
              updateAction : "$_padlistUpdateAction()$"
        }
      , loginModal : {
             login : "$_login()$"
           , email : "$_email()$"
           , password : "$_password()$"
           , forgot : "$_forgot()$"
           , sendNewPassword : "$_loginModalForgotPasswordSendNewPassword()$"
           , loginFailed : "$_flashMessageLoginPageRedirectReasonWrongMailOrPassword()$"
           , loginFailedBadIP : "$_flashMessageLoginPageRedirectReasonWrongIPAddress()$"
           , invalidEmail : "$_flashEmailIncorectlyFormed()$"
           , passwordReminderMaybeSend : "$_flashMessageChangePasswordEmailMaybeSent()$"
           , dontHaveAccount : "$_dontHaveAccount()$"
      }
      , blocking : {
          free: {
            create : {
              title  : "$_blockingFreeCreateTitle()$"
            },
            has : {
              headline : "$_blockingFreeHasHeadline()$",
              subtext1 : "$_blockingFreeHasSubtext1()$"
            },
            hasNot : {
              headline : "$_blockingFreeHasNotHeadline()$",
              subtext1 : "$_blockingFreeHasNotSubtext1()$"
            }
          },
          usedall : {
            headline : "$_blockingUsedAllHeadline()$",
            subtext1 : "$_blockingUsedAllSubtext1()$"
          }
        }
      , apiConfiration : {
             title : "$_apiConfiramtionTitle()$"
           , header : "$_apiConfiramtionHeader()$"
           , footerNotLogged : "$_apiConfiramtionFooterNotLogged()$"
           , footerLogged : "$_apiConfiramtionFooterLogged()$"
           , readPermission : "$_apiConfiramtionReadPermission()$"
           , createPermission : "$_apiConfiramtionCreatePersmission()$"
           , readUserInfo : "$_apiConfiramtionReadUserInfo()$"
           , sendPermission : "$_apiConfiramtionSendPermission()$"
           , accept : "$_apiConfiramtionAccept()$"
           , loginAndAcceptTitle : "$_apiConfiramtionLoginAndAcceptTitle()$"
           , loginAndAccept : "$_apiConfiramtionLoginAndAccept()$"
           , createAccount : "$_apiConfiramtionCreateAccount()$"
           , enterEmailAdress : "$_apiConfiramtionEnterEmailAdress()$"
      }
      , apiDashboard : {
            reveal : "$_apiOauthReveal()$"
          , deleteToken :  "$_apiDashboardDelete()$"
          , grantedPriviligesDescription: "$_apiDashboardGrantedPriviligesDescription()$"
          , personalTokenDescription: "$_apiDashboardpersonalTokenDescription()$"
          , personalTokenCreate: "$_apiDashboardPersonalTokenCreate()$"
          , apiTokenDescription : "$_apiDashboardApiTokenDescription()$"
          , apiTokenCreate  : "$_apiDashboardApiTokenCreate()$"
          , personOrCompany : "$_apiDashboardPersonOrCompany()$"
          , privilige       : "$_apiDashboardPrivilige()$"
          , personalTokens  : "$_apiDashboardPersonalToken()$"
          , grandedTokens   : "$_apiDashboardGrantedPriviliges()$"
          , apiTokens       : "$_apiDashboardApiToken()$"
      }
      , account :{
            accountDetails :  {
                name : "$_HeadUserInfo()$"
              , fstname : "$_firstname()$"
              , sndname : "$_lastname()$"
              , fullname : "$_fullName()$"
              , personnumber : "$_personnumber()$"
              , email : "$_email()$"
              , phone : "$_phone()$"
              , companyname : "$_company()$"
              , companynumber : "$_orgnumber()$"
              , companyposition : "$_companyposition()$"
              , companyaddress : "$_address()$"
              , companyzip :     "$_zip()$"
              , companycity :    "$_city()$"
              , companycountry : "$_country()$"
              , newEmail       : "$_newemail()$"
              , newEmailAgain  : "$_newemailagain()$"
              , changeEmailCurrentEmail : "$_currentemailaddress()$"
              , changeEmailButton : "$_changeEmailRequestButtonText()$"
              , changeEmailExplaination : "$_changeEmailRequestExplanation()$"
              , changeEmailTitle : "$_changeEmailRequestTitle()$"
              , changeEmailAccept : "$_changeEmailRequestAcceptText()$"
              , save : "$_SaveChanges()$"
              , detailSaved : "$_flashMessageUserDetailsSaved()$"
              , mismatchedEmails : "$_flashMessageMismatchedEmails()$"
              , invalidEmail : "$_flashMessageInvalidEmailFormat()$"
              , emailAlreadyInUse : "$_flashCantChangeEmail()$"
              , changeEmailMailSent : "$_flashMessageChangeEmailMailSent()$"
            }
          , apiSettings : {
                name : "$_apiSettings()$"
          }
          , accountSecurity : {
                passwordSection : "$_password()$"
              , oldpassword     : "$_PresentPassword()$"
              , newpassword1    : "$_NewPassword()$"
              , newpassword2    : "$_RepeatNewPassword()$"
              , lang            : "$_UserLanguage()$"
            }
          , companySettings : "$_CompanySettings()$"
          , companyAccounts : {
                name : "$_CompanyAccounts()$"
              , createNewButtonText : "$_newCompanyAccountButtonText()$"
              , createNewModalBody : "$_modalAddCompanyAccountBody()$"
              , createNewModalTitle:"$_modalAddCompanyAccountTitle()$"
              , createNewModalAcceptButton: "$_modalAddCompanyAccountInviteButtonText()$"
              , search : "$_SearchCompanyAccount()$"
              , roleAdmin : "$_admin()$"
              , roleStandard : "$_standard()$"
              , rolePending : "$_pendingcompanyaccount()$"
              , resendModalBody : "$_modalResendCompanyAccountInviteBody()$ "
              , resendModalTitle : "$_modalResendCompanyAccountInviteHeader()$"
              , resendModalAccept : "$_modalResendCompanyAccountInviteConfirm()$"
              , deleteModalBody : "$_listLocalDialogsBody()$ "
              , deleteModalTitle : "$_delete()$"
              , deleteFailedHasDocuments : "$_flashMessageUserHasLiveDocs()$"
              , companyInviteSent : "$_flashCompanyAccountInviteSent()$"
              , companyInviteNotSent : "$_flashCompanyAccountInviteNotSent()$"
              , companyInviteNotSentSameCompany: "$_flashCompanyInviteNotSentSameCompany()$"
              , companyInviteResent : "$_flashCompanyAccountInviteResent()$"
              , companyAccountDeleted : "$_flashMessageCompanyAccountDeleted()$"
              , columnName :  "$_name()$"
              , columnEmail : "$_email()$"
              , columnRole :  "$_role()$"

            }
          , stats : {
                name : "$_HeadUsageStats()$"
              , last30days : "$_last30Days()$"
              , last6months : "$_last6Months()$"
              , columnDate: "$_usageStatsDate()$"
              , columnMonth : "$_usageStatsMonth()$"
              , columnSender : "$_statsSender()$"
              , columnClosedDocuments : "$_closedDocuments()$"
              , columnSendDocuments :"$_sentDocuments()$"
              , columnClosedSignatures : "$_closedSignatures()$"
            }
          , subscription : "$_Subscription()$"
      }
      , sampleSignView : {
            phone : "$_sampleSignViewPhone()$"
          , email : "$_sampleSignViewEmail()$"
          , rejectButton : "$_sampleSignViewRejectButton()$"
          , rejectWithoutReasonButton : "$_sampleSignViewRejectWithoutReasonButton()$"
          , signButton : "$_sampleSignViewSignButton()$"
          , signViewHeader : "$_sampleSignViewHeader()$"
      }
      , companyBranding : {
             brandingPreview: {
                   emailButtonLabel : "$_sampleEmailButtonLabel()$"
                 , emailContent: "$_SampleEmailContent()$"
                 , emailInstructions: "$_SampleEmailInstructions()$"
                 , emailFooter : "$_sampleEmailFooter()$"
             }
      }
      , fontSize: {
               name : "$_fontSizeName()$"
             , small :  "$_fontSizeSmall()$"
             , normal :  "$_fontSizeNormal()$"
             , big :  "$_fontSizeBig()$"
             , large :  "$_fontSizeLarge()$"
             , custom :  "$_fontSizeCustom()$"
      }
      , checkboxSize: {
               name : "$_checkboxSizeName()$"
             , small :  "$_checkboxSizeSmall()$"
             , medium :  "$_checkboxSizeMedium()$"
             , large :  "$_checkboxSizeLarge()$"
      }
      , dates : {
               jan       : "$_datesJan()$"
             , january   : "$_datesJanuary()$"
             , feb       : "$_datesFeb()$"
             , february  : "$_datesFebruary()$"
             , mar       : "$_datesMar()$"
             , march     : "$_datesMarch()$"
             , apr       : "$_datesApr()$"
             , april     : "$_datesApril()$"
             , may       : "$_datesMay()$"
             , mayy      : "$_datesMayy()$"
             , jun       : "$_datesJun()$"
             , june      : "$_datesJune()$"
             , jul       : "$_datesJul()$"
             , july      : "$_datesJuly()$"
             , aug       : "$_datesAug()$"
             , august    : "$_datesAugust()$"
             , sep       : "$_datesSep()$"
             , september : "$_datesSeptember()$"
             , oct       : "$_datesOct()$"
             , october   : "$_datesOctober()$"
             , nov       : "$_datesNov()$"
             , november  : "$_datesNovember()$"
             , dec       : "$_datesDec()$"
             , december  : "$_datesDecember()$"
      }
      , createFromTemplateDescription : "$_createFromTemplateDescription()$"
      , delivery : {
               email        : "$_deliveryEmail()$"
             , pad          : "$_deliveryPad()$"
             , mobile       : "$_deliveryMobile()$"
             , email_mobile : "$_deliveryEmailMobile()$"
             , api          : "$_deliveryAPI()$"
      }
      , signinginprogressmodal : {
               action1 : "$_signinginprogressmodalAction1()$"
             , action2 : "$_signinginprogressmodalAction2()$"
      }
      , signingInProgressDontCloseWindow : "$_signingInProgressDontCloseWindow()$"
      , signingStartedDontCloseWindow : "$_signingStartedDontCloseWindow()$"
      , signingErrorMessage1 : "$_signingErrorMessage1()$"
      , signingErrorMessage2 : "$_signingErrorMessage2()$"
      , signingErrorReload   : "$_signingErrorReload()$"
      , integrations: {
              salesforce : "$salesforce()$"
            , salesforceDescription : "$salesforceDescription()$"
            , basicAuth : "$basicAuth()$"
            , basicAuthDescription : "$basicAuthDescription()$"
            , constantUrlCallback : "$constantUrlCallback()$"
            , constantUrlCallbackDescription : "$constantUrlCallbackDescription()$"
      }
      , footer : {
              explore              : "$_pageFooterExplore()$"
            , api                  : "$_pageFooterAPI()$"
            , apiExplorer          : "$_pageFooterAPIExplorer()$"
            , jobs                 : "$_pageFooterWork()$"
            , verify               : "$_footerVerifyDocument()$"
            , getStarted           : "$_headerGetStarted()$"
            , terms                : "$_pageFooterTerms()$"
            , tos                  : "$_pageFooterTOS()$"
            , environmentalPolicy  : "$_pageFooterEnvironmentalPolicy()$"
            , privacyPolicy        : "$_pageFooterPrivacyPolicy()$"
            , contact              : "$_pageFooterContact()$"
            , workday              : "$_pageFooterWorkDay()$"
            , address              : "$_pageFooterAddress()$"
            , address1             : "$_pageFooterAddress1()$"
            , address3             : "$_pageFooterAddress3()$"
            , address4             : "$_pageFooterAddress4()$"
            , blog                 : "$_pageFooterBlog()$"

      }
      , header : {
              login : "$_login()$"
            , startFreeTrial : "$_startFreeTrial()$"
            , partners : "$_headerPartners()$"
            , contact : "$_headerContakt()$"
            , about : "$_headerAbout()$"
            , cases : "$_headerCases()$"
            , pricing : "$_headerPricing()$"
            , legal :  "$_headerLegal()$"
            , features : "$_headerFeatures()$"
            , logout :  "$_headerLogOut()$"
            , account : "$_headerAccount()$"
            , archive : "$_headerArchive()$"
            , template : "$_headerTemplate()$"
            , send     : "$_headerSend()$"
      }
      , branding: {
            defaultTheme : "$_brandingDefaultTheme()$"
          , newTheme : "$_brandingNewTheme()$"
          , newThemeWithDots : "$_brandingNewThemeWithDots()$"
          , enterNameOfThemeBellow : "$_brandingEnterNameOfThemeBellow()$"
          , save : "$_brandingSave()$"
          , createCustomThemeTitle : "$_brandingCreateCustomThemeTitle()$"
          , createCustomThemeDescription : "$_brandingCreateCustomThemeDescription()$"
          , doYouWantToDeleteTheme : "$_brandingDoYouWantToDeleteTheme()$"
          , deleteTheme : "$_brandingDeleteTheme()$"
          , deleteNow : "$_delete()$"
          , themeDeleted : "$_brandingThemeDeleted()$"
          , saved : "$_brandingSaved()$"
          , unsavedChangesTitle: "$_brandingUnsavedChangesTitle()$"
          , unsavedChangesDescription : "$_brandingUnsavedChangesDescription()$"
          , saveNow : "$_brandingSaveNow()$"
          , discard : "$_brandingDiscard()$"
          , themesTitle : "$_brandingThemesTitle()$"
          , settingsTitle : "$_brandingSettingsTitle()$"
          , emailThemeTitle : "$_brandingEmailThemeTitle()$"
          , signviewThemeTitle : "$_brandingSignviewThemeTitle()$"
          , serviceThemeTitle : "$_brandingServiceThemeTitle()$"
          , loginThemeTitle : "$_brandingLoginThemeTitle()$"
          , useExistingOrCreateNewTheme : "$_brandingUseExistingOrCreateNewTheme()$"
          , createNewTheme : "$_brandingCreateNewTheme()$"
          , themes: {
              name : "$_brandingThemesName()$"
            , logo : "$_brandingThemesLogo()$"
            , uploadNewLogo : "$_brandingThemesUploadNewLogo()$"
            , brandColor : "$_brandingThemesBrandColor()$"
            , brandColorSample : "$_brandingThemesBrandColorSample()$"
            , actionColor : "$_brandingThemesActionColor()$"
            , actionColorSample : "$_brandingThemesActionColorSample()$"
            , actionSecondaryColor : "$_brandingThemesActionSecondaryColor()$"
            , actionSecondaryColorSample : "$_brandingThemesActionSecondaryColorSample()$"
            , positiveColor : "$_brandingThemesPositiveColor()$"
            , positiveColorSample : '$_brandingThemesPositiveColorSample()$'
            , negativeColor : "$_brandingThemesNegativeColor()$"
            , negativeColorSample : '$_brandingThemesNegativeColorSample()$'
            , font: "$_brandingThemesFont()$"
            , fontSample : "$_brandingThemesFontSample()$"
            , deleteTitle : "$_brandingThemesDeleteTitle()$"
            , deleteNow : "$_brandingThemesDelete()$"
            , emailPreview : "$_brandingThemesEmailPreview()$"
            , signviewPreview: "$_brandingThemesSignviewPreview()$"
            , servicePreview: "$_brandingThemesServicePreview()$"
            , loginPreview: "$_brandingThemesLoginlPreview()$"
          }
          , companySettings : {
              browserTitle : "$_brandingCompanySettingsBrowserTitle()$"
            , browserTitleDescription : "$_brandingCompanySettingsBrowserTitleDescription()$"
            , smsOriginator : "$_brandingCompanySettingsSmsOriginator()$"
            , smsOriginatorDescription : "$_brandingCompanySettingsSmsOriginatorDescription()$"
            , favicon : "$_brandingCompanySettingsFavicon()$"
            , faviconDescription : "$_brandingCompanySettingsFaviconDescription()$"
            , uploadFavicon : "$_brandingCompanySettingsUploadFavicon()$"
          }
      },
      notNamedParty : "$_notNamedParty()$",
      imageFileFormatNotSpportedIco : "$_imageFileFormatNotSpportedIco()$",
      imageFileFormatNotSpported : "$_imageFileFormatNotSpported()$"
    };
###
