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
             , signviewApproveConfirmation: "$_contractApproveConfirmation()$"
             , signviewConfirmationSignaturesPlaced: "$_contractSignConfirmationSignaturePlaced()$"
             , signviewLegalAgreement: {
                   textTop: "$_signviewLegalAgreementText()$"
                 , textBottom: "$_signviewLegalAgreementTextBottom()$"
                 , point1: "$_signviewLegalAgreementPoint1()$"
                 , point2: "$_signviewLegalAgreementPoint2()$"
                 , point3: "$_signviewLegalAgreementPoint3()$"
               }
             , signviewPdfOrPhoto: "$_pdfOrPhoto()$"
             , identifyDocument: "$_IdentifyDocument()$"
             , identifyBankId: "$_IdentifyBankId()$"
             , identifyBankIdNo: "$_IdentifyBankIdNO()$"
             , identifyBankIdNoMobile: "$_IdentifyBankIdNOMobile()$"
             , identifyNOBankIdError: {
               failed: "$_IdentifyBankIdNOFailed()$"
               , canceled: "$_IdentifyBankIdNOCanceled()$"
               , useragent: "$_IdentifyBankIdNOUserAgent()$"
               , auth: "$_IdentifyBankIdNOAuth()$"
               , blocked: "$_IdentifyBankIdNOBlocked()$"
               , revoked: "$_IdentifyBankIdNORevoked()$"
               , expired: "$_IdentifyBankIdNOExpired()$"
               , mobile: "$_IdentifyBankIdNOMobileError()$"
             }
             , identifyWithPin: {
                invalidPin: "$_identifyWithPinInvalidPin()$"
                , enterPin: "$_identifyWithPinEnterPin()$"
                , pinInfotext: "$_identifyWithPinInfotext()$"
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
             , identifyFITupasError: {
                 failed: "$_IdentifyFITupasFailed()$"
               , canceled: "$_IdentifyFITupasCanceled()$"
               , useragent: "$_IdentifyFITupasUserAgent()$"
               , auth: "$_IdentifyFITupasAuth()$"
               , blocked: "$_IdentifyFITupasBlocked()$"
               , revoked: "$_IdentifyFITupasRevoked()$"
               , expired: "$_IdentifyFITupasExpired()$"
             }
             , identify: {
                 authorizationDataMismatch : "$_identifyVerimiDataMismatch()$"
               , identifyWithVerimi: "$_identifyWithVerimi()$"
               , identifyWithIdin: "$_identifyWithIdin()$"
             }
             , signNOBankIDError : {
                 generic   : "$_SignBankIdNOErrorGeneric()$"
               , useragent : "$_SignBankIdNOErrorUserAgent()$"
               , auth      : "$_SignBankIdNOErrorAuth()$"
               , document  : "$_SignBankIdNOErrorDocument()$"
               , timeout   : "$_SignBankIdNOErrorTimeout()$"
               , age       : "$_SignBankIdNOErrorAge()$"
               , mobiledob : "$_SignBankIdNOErrorMobileDOB()$"
             }
             , signDKNemIDError : {
                 generic   : "$_SignNemIdDKErrorGeneric()$"
               , useragent : "$_SignNemIdDKErrorUserAgent()$"
               , auth      : "$_SignNemIdDKErrorAuth()$"
               , document  : "$_SignNemIdDKErrorDocument()$"
               , timeout   : "$_SignNemIdDKErrorTimeout()$"
               , age       : "$_SignNemIdDKErrorAge()$"
               , mobiledob : "$_SignNemIdDKErrorMobileDOB()$"
             }
             , signNOBankIDSuccess : "$_SignBankIdNOSuccess()$"
             , signNOBankIDCanceled : "$_SignBankIdNOCanceled()$"
             , signDKNemIDSuccess : "$_SignNemIdDKSuccess()$"
             , signDKNemIDCanceled : "$_SignNemIdDKCanceled()$"
             , identifyPhoneError: "$_IdentifyPhoneError()$"
             , identifyPhoneNumber: "$_IdentifyPhoneNumber()$"
             , identifyPhoneNumber8: "$_IdentifyPhoneNumber8()$"
             , identifyDateOfBirth: "$_IdentifyDateOfBirth()$"
             , openBankId: "$_OpenBankId()$"
             , yourIdNumber: "$_YourIdNumber()$"
             , yourMobileNumber:"$_YourMobileNumber()$"
             , yourEmail:"$_YourEmail()$"
             , idNumber: "$_IdNumber()$"
             , verifyIdentityWithName : "$_VerifyIdentityWithName()$"
             , verifyIdentityWithoutName : "$_VerifyIdentityWithoutName()$"
             , problemContactingServer : "$_problemContactingServer()$"
             , esigningpoweredbyscrive : "$PoweredByScrive()$"
             , sessionTimedoutInSignview : "$_sessionTimedoutInSignview()$"
             , failedToStartNewEIDProcess: "$_failedToStartNewEIDProcess()$"
             , signviewSigningFailed : "$_signviewSigningFailed()$"
             , signviewForwardingFailed: "$_signviewForwardingFailed()$"
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
                 , confirmationdeliveryproblem : "$_tooltipconfirmationdeliveryproblem()$"
             }
             , listand : "$_listand()$"
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
             , forward : {
                         forward : "$_forwardForward()$"
                       , forwardtootherperson : "$_forwardToOtherPerson()$"
                       , forwardtootherpersonapproving : "$_forwardToOtherPersonApproving()$"
                       , messageforotherperson : "$_forwardMessageForOtherPerson()$"
                       , typeyourmessagehere : "$_forwardTypeYourMessageHere()$"
                       , otherpersondetails : "$_forwardOtherPersonDetails()$"
                       , send : "$_forwardSend()$"
                       , forwardedConfirmationTitle : "$_forwardedConfirmationTitle()$"
                       , forwardedConfirmationThankYou : "$_forwardedConfirmationThankYou()$"
                       , forwardedConfirmationDescriptionSigning : "$_forwardedConfirmationDescriptionSigning()$"
                       , forwardedConfirmationDescriptionApproving : "$_forwardedConfirmationDescriptionApproving()$"
                       , forwardedConfirmationClosePage : "$_forwardedConfirmationClosePage()$"
               }
             , signatoryMessage : {
                         rejected : "$_signatoryMessageIfSignatoryRejected()$"
                       , seen : "$_signatoryMessageIfSignatorySeendate()$"
                       , read : "$_signatoryMessageIfSignatoryReaddate()$"
                       , delivered : "$_signatoryMessageIfSignatoryDeliveredEmail()$"
                       , signed : "$_signatoryMessageIfSigned()$"
                       , approved : "$_signatoryMessageIfApproved()$"
                       , other : "$_signatoryMessageIfElse()$"
                       , waiting : "$_signatoryMessageWaiting()$"
                       , deliveryproblem : "$_signatoryMessageDeliveryProblem()$"
                       , confirmationdeliveryproblem : "$_signatoryConfirmationMessageDeliveryProblem()$"
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
               forwardtext: "$_forwardToAnotherParty()$",
               rejectbuttontext: "$_contractrejectbuttontext()$",
               rejectbuttontextwithoutreason: "$_contractrejectbuttontextwithoutreason()$",
               approvebuttontext: "$_contractapprovebuttontext()$",
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
                , required : "$_authorAttachmentRequired()$"
                , optional : "$_authorAttachmentOptional()$"
                , nameOfAttachment : "$_authorAttachmentName()$"
                , typeOfAttachment :  "$_authorAttachmentType()$"
                , back : "$_authorAttachmentBack()$"
                , add : "$_authorAttachmentAdd()$"
                , mergedQuestion : "$_authorAttachmentMergedQuestion()$"
                , merged : "$_authorAttachmentMerged()$"
                , notMerged : "$_authorAttachmentNotMerged()$"
                , uploadingCounter: "$_authorAttachmentsUploadingCounter()$"
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
              , typeOfAttachment:   "$_pageDocumentForAuthorHelpersLocalDialogsAttachmenttype()$"
              , required:           "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentrequired()$"
              , optional:           "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentoptional()$"
              , markAsNotUploaded:  "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentmarkasnotuploaded()$"
              , wasNotUploaded:     "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentwasnotuploaded()$"
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
              , cs : "$_Czech()$"
              , pl : "$_Polish()$"
              , hu : "$_Hungarian()$"
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
              , csInCs : "$CzechInCzech()$"
              , plInPl : "$PolishInPolish()$"
              , huInHu : "$HungarianInHungarian()$"
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
                , showarrow: "$_signviewSettingsShowArrow()$"
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
               , invitationLink : "$_designViewAddPartiesInvitationLink()$"
               , invitationPortal: "Portal"
               , role : "$_designViewAddPartiesRole()$"
               , roleSignatory : "$_designViewAddPartiesRoleSignatory()$"
               , roleViewer : "$_designViewAddPartiesRoleViewer()$"
               , roleApprover : "$_designViewAddPartiesRoleApprover()$"
               , authenticationToView : "$_designViewAddPartiesAuthenticationToView()$"
               , authenticationToViewArchived : "$_designViewAddPartiesAuthenticationToViewArchived()$"
               , authenticationToViewStandard : "$_designViewAddPartiesAuthenticationToViewStandard()$"
               , authenticationToViewSEBankID : "$_designViewAddPartiesAuthenticationtoViewSEBankID()$"
               , authenticationToViewNOBankID : "$_designViewAddPartiesAuthenticationtoViewNOBankID()$"
               , authenticationToViewDKNemID : "$_designViewAddPartiesAuthenticationtoViewDKNemID()$"
               , authenticationToViewFITupas : "$_designViewAddPartiesAuthenticationtoViewFITupas()$"
               , authenticationToViewSMSPin : "$_designViewAddPartiesAuthenticationSMSPin()$"
               , authenticationToViewVerimi : "$_verimiText()$"
               , authenticationToViewIDIN : "$_idinText()$"
               , authenticationToSign : "$_designViewAddPartiesAuthentication()$"
               , authenticationToSignStandard : "$_designViewAddPartiesAuthenticationStandard()$"
               , authenticationToSignSEBankID : "$_designViewAddPartiesAuthenticationSEBankID()$"
               , authenticationToSignNOBankID : "$_designViewAddPartiesAuthenticationNOBankID()$"
               , authenticationToSignDKNemID : "$_designViewAddPartiesAuthenticationDKNemID()$"
               , authenticationToSignSMSPin : "$_designViewAddPartiesAuthenticationSMSPin()$"
               , authenticationToSignIDIN : "$_idinText()$"
               , confirmation : "$_designViewAddPartiesConfirmation()$"
               , confirmationEmail : "$_designViewAddPartiesConfirmationEmail()$"
               , confirmationSMS : "$_designViewAddPartiesConfirmationSMS()$"
               , confirmationEmailSMS : "$_designViewAddPartiesConfirmationEmailSMS()$"
               , confirmationNone : "$_designViewAddPartiesConfirmationNone()$"
               , confirmationLink : "$_designViewAddPartiesConfirmationLink()$"
               , confirmationAttachments : "$_designViewAddPartiesConfirmationAttachments()$"
               , secondaryConfirmation : "$_designViewAddPartiesSecondaryConfirmation()$"
               , allowHighlighting: "$_designViewAddPartiesAllowHighlighting()$"
               , allowHighlightingTooltip: "$_designViewAddPartiesAllowHighlightingTooltip()$"
               , allowForward: "$_designViewAddPartiesAllowForward()$"
               , allowForwardTooltip: "$_designViewAddPartiesAllowForwardTooltip()$"
               , notifications: "$_designViewAddPartiesNotifications()$"
               , notificationsNone: "$_designViewAddPartiesNotificationsNone()$"
               , notificationsEmail: "$_designViewAddPartiesNotificationsEmail()$"
               , notificationsSMS: "$_designViewAddPartiesNotificationsSMS()$"
               , notificationsEmailSMS: "$_designViewAddPartiesNotificationsEmailSMS()$"
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
             , notAllowedToChangeAuthenticationMethod: "$_designViewNotAllowedToChangeAuthenticationMethod()$"
             , removePage : {
                 modalTitle : "$_designViewRemovePageModalTitle()$"
               , modalBody1 : "$_designViewRemovePageModalBody1()$"
               , modalBody2 : "$_designViewRemovePageModalBody2()$"
               , modalRemoveButton : "$_designViewRemovePageModalButton()$"
             }
             , dropToUpload: "$_designViewDropToUpload()$"
             , dropPDFHere: "$_designViewDropPDFHere()$"
             , radiogroup: "$_radioGroupFieldName()$"
             , radiobutton: "$_radioButtonPlacementName()$"
             , radioButtonNames: "$_radioButtonNames()$"
             , radioButtonNamesNotUnique: "$_radioButtonNamesNotUnique()$"
             , customValidation : {
                 custom: "$_designViewCustomValidationCustom()$"
               , fieldValidation: "$_designViewCustomValidationFieldValidation()$"
               , pattern: "$_designViewCustomValidationPattern()$"
               , patternName : {
                   email: "$_designViewCustomValidationPatternNameEmail()$"
                 , numbers: "$_designViewCustomValidationPatternNameNumbers()$"
                 , phone: "$_designViewCustomValidationPatternNamePhone()$"
               }
               , patternPlaceholder: "$_designViewCustomValidationPatternPlaceholder()$"
               , remainingCharacters: "$_designViewCustomValidationRemainingCharacters()$"
               , tooltipMessage: "$_designViewCustomValidationTooltipMessage()$"
               , tooltipMessagePlaceholder: "$_designViewCustomValidationTooltipMessagePlaceholder()$"
               , validation: "$_designViewCustomValidationValidation()$"
               , validExample: "$_designViewCustomValidationValidExample()$"
               , validExamplePlaceholder: "$_designViewCustomValidationValidExamplePlaceholder()$"
             }
         }
         , validation : {
                required: "$_nameRequired()$"
              , toolong: "$_nameTooLong()$"
              , invalidnamechars: "$_nameInvalidChars()$"
              , passwordExceedsMaxLength: "$_PasswordValidationMessageInputExceedsMaxLength()$"
              , passwordLessThanMinLength: "$_PasswordValidationMessageInputLessThanMinLength()$"
              , passwordsDontMatch: "$_PasswordValidationMessagePasswordsDontMatch()$"
              , passwordCantBeUsed: "$_PasswordCantBeUsed()$"
              , mustAcceptTOS: "$_flashMessageMustAcceptTOS()$"
              , wrongEmail : "$_emptyOrNotValidEmail()$"
              , passwordOldPasswordNotValid : "$_flashMessageBadOldPassword()$"
              , wrongName : "$_flashWrongName()$"
              , wrongCountry : "$_flashWrongCountry()$"
              , wrongCity : "$_flashWrongCity()$"
              , wrongZip : "$_flashWrongZip()$"
              , wrongCompanyAddress : "$_flashWrongCompanyAddress()$"
              , wrongCompanyNumber : "$_flashWrongCompanyNumber()$"
              , wrongCompanyName : "$_flashWrongCompanyName()$"
              , wrongPosition : "$_flashWrongPosition()$"
              , wrongPersonalNumber : "$_flashWrongPersonalNumber()$"
              , wrongPhone : "$_flashWrongPhone()$"
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
           , invalidEmailFlash: "$_changeEmailModalAcceptInvalidEmailFlash()$"
           , placeholder: "$_changeEmailModalPlaceholder()$"
         }
         , changeMobileModal: {
             title: "$_changeMobileModalTitle()$"
           , label: "$_changeMobileModalLabel()$"
           , invalidMobileFlash: "$_changeMobileModalAcceptInvalidMobileFlash()$"
           , placeholder: "$_changeMobileModalPlaceholder()$"
         }
         , changeEmailAndPhone : "$_changeEmailAndPhone()$"
         , changeEmailAndMobileModal: {
             title: "$_changeEmailAndMobileModalTitle()$"
           , label: "$_changeEmailAndMobileModalLabel()$"
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
             , optionallyRequestedFrom : "$_authorviewOptionallyRequestedFrom()$"
             , notUploaded : "$_authorviewNotUploaded()$"
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
             , readAndSign: "$_readAndSign()$"
             , readAndSignWithUserName: "$_readAndSignWithUserName()$"
             , reviewDocument: "$_reviewDocument()$"
             , signedNotClosed: "$_signedNotClosed()$"
             , signedAndClosed: "$_signedAndClosed()$"
             , approved: "$_approved()$"
             , unavailableForSign: "$_unavailableForSign()$"
             , dueDate: "$_dueDate()$"
             , companyNumberLabel: "$_companyNumberLabel()$"
             , personalNumberLabel: "$_personalNumberLabel()$"
             , authorAttachmentsUnderstoodContent : "$_authorAttachmentsUnderstoodContent()$"
             , initiatorTitle: "$_signViewInitiatorTitle()$"
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
                , viewingApproving : "$_controlsStatusViewingApproving()$"
                , viewingClosed    : "$_controlsStatusViewingClosed()$"
                , viewingCanceled  : "$_controlsStatusViewingCanceled()$"
                , viewingRejected  : "$_controlsStatusViewingRejected()$"
                , viewingTimedout : "$_controlsStatusViewingTimedout()$"
                , authornamePlaceholder : "$_authorParty()$"
             }
             , clearHighlightingHere: "$_clearHighlightingHere()$"
             , doneHighlighting: "$_doneHighlighting()$"
             , eleg : {
                  bankid : {
                        faultModalTitle : "$_elegBankIDFaultModalTitle()$"
                      , startAppNow : "$_elegBankIDStartAppNow()$"
                      , transactionNumber : "$_elegSignTransactionNumber()$"
                      , signChooseMethod : "$_elegSignChooseMethod()$"
                      , signSEConfirmationTitle : "$_elegSignSEConfirmationTitle()$"
                      , signNLConfirmationTitle : "$_elegSignNLConfirmationTitle()$"
                      , signNOConfirmationTitle : "$_elegSignNOConfirmationTitle()$"
                      , signDKConfirmationTitle : "$_elegSignDKConfirmationTitle()$"
                      , signSEConfirmationText : "$_elegSignSEConfirmationText()$"
                      , signNOConfirmationText : "$_elegSignNOConfirmationText()$"
                      , signNOSignMethodClassic : "$_elegSignNOSignMethodClassic()$"
                      , signNOSignMethodMobile : "$_elegSignNOSignMethodMobile()$"
                      , signDKConfirmationText : "$_elegSignDKConfirmationText()$"
                      , signDKSignMethodEmployeeKeycard : "$_elegSignDKSignMethodEmployeeKeycard()$"
                      , signDKSignMethodEmployeeKeyfile : "$_elegSignDKSignMethodEmployeeKeyfile()$"
                      , signDKSignMethodPersonalKeycard : "$_elegSignDKSignMethodPersonalKeycard()$"
                      , signSEConfirmationTextNoName : "$_elegSignSEConfirmationTextNoName()$"
                      , signNOConfirmationTextNoName : "$_elegSignNOConfirmationTextNoName()$"
                      , signDKConfirmationTextNoName : "$_elegSignDKConfirmationTextNoName()$"
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
                , nets : {
                    errorExpiredTransaction : "$_elegNetsErrorExpiredTransaction()$"
                  , errorGeneric : "$_elegNetsErrorGeneric()$"
                  , errorAPI : "$_elegNetsErrorAPI()$"
                  , errorCanceledByMerchant: "$_elegNetsErrorCanceledByMerchant()$"
                  , errorExpired : "$_elegNetsErrorExpired()$"
                  , errorExpiredByProxy: "$_elegNetsErrorExpiredByProxy()$"
                  , errorRejectedBySigner: "$_elegNetsErrorRejectedBySigner()$"
                  , transactionInProgress: "$_elegNetsTransactionInProgress()$"
                  }
             }
        }
        , docview : {
            signatory : {
                editAuthenticationToViewMethod: "$_docviewSignatoryeditAuthenticationToViewMethod()$"
              , editAuthenticationToSignMethod: "$_docviewSignatoryeditAuthenticationToSignMethod()$"
              , editAuthenticationToViewArchivedMethod: "$_docviewSignatoryeditAuthenticationToViewArchivedMethod()$"
              , showLinkDelivery: "$_docviewSignatoryShowLinkDelivery()$"
              , invitationOrder: "$_docviewSignatoryInvitationOrderLabel()$"
              , invitationMethod: "$_docviewSignatoryInvitationMethodLabel()$"
              , invitationEmail: "$_docviewSignatoryInvitationEmail()$"
              , invitationPad: "$_docviewSignatoryInvitationPad()$"
              , invitationSMS: "$_docviewSignatoryInvitationSMS()$"
              , invitationEmailSMS: "$_docviewSignatoryInvitationEmailSMS()$"
              , invitationLink: "$_docviewSignatoryInvitationLink()$"
              , invitationNone: "$_docviewSignatoryInvitationNone()$"
              , invitationPortal: "Portal"
              , role: "$_docviewSignatoryRoleLabel()$"
              , roleApprover: "$_docviewSignatoryRoleApprover()$"
              , roleSignatory: "$_docviewSignatoryRoleSignatory()$"
              , roleViewer: "$_docviewSignatoryRoleViewer()$"
              , authenticationToView: "$_docviewSignatoryAuthenticationToViewMethodLabel()$"
              , authenticationToViewArchived: "$_docviewSignatoryAuthenticationToViewArchivedMethodLabel()$"
              , authenticationToViewStandard: "$_docviewSignatoryAuthenticationToViewStandard()$"
              , authenticationToViewSEBankID: "$_docviewSignatoryAuthenticationToViewSEBankID()$"
              , authenticationToViewNOBankID: "$_docviewSignatoryAuthenticationToViewNOBankID()$"
              , authenticationToViewDKNemID: "$_docviewSignatoryAuthenticationToViewDKNemID()$"
              , authenticationToViewFITupas: "$_docviewSignatoryAuthenticationToViewFITupas()$"
              , authenticationToViewSMSPin:  "$_docviewSignatoryAuthenticationToViewSMSPin()$"
              , authenticationToViewVerimi: "$_verimiText()$"
              , authenticationToViewIDIN: "$_idinText()$"
              , authenticationToSign: "$_docviewSignatoryAuthenticationToSignMethodLabel()$"
              , authenticationToSignStandard: "$_docviewSignatoryAuthenticationToSignStandard()$"
              , authenticationToSignSMSPin: "$_docviewSignatoryAuthenticationToSignSMSPin()$"
              , authenticationToSignNOBankID: "$_docviewSignatoryAuthenticationToSignNOBankID()$"
              , authenticationToSignDKNemID: "$_docviewSignatoryAuthenticationToSignDKNemID()$"
              , authenticationToSignSEBankID: "$_docviewSignatoryAuthenticationToSignSEBankID()$"
              , authenticationToSignIDIN: "$_idinText()$"
              , confirmation: "$_docviewSignatoryConfirmationMethodLabel()$"
              , confirmationEmail: "$_docviewSignatoryConfirmationEmail()$"
              , confirmationSMS: "$_docviewSignatoryConfirmationSMS()$"
              , confirmationEmailSMS: "$_docviewSignatoryConfirmationEmailSMS()$"
              , confirmationNone: "$_docviewSignatoryConfirmationNone()$"
              , notification: "$_docviewSignatoryNotificationMethodLabel()$"
              , notificationEmail: "$_docviewSignatoryNotificationEmail()$"
              , notificationSMS: "$_docviewSignatoryNotificationSMS()$"
              , notificationEmailSMS: "$_docviewSignatoryNotificationEmailSMS()$"
              , notificationNone: "$_docviewSignatoryNotificationNone()$"
              , secondaryConfirmation: "$_docviewSignatorySecondaryConfirmation()$"
              , secondaryConfirmationAttachments: "$_docviewSignatorySecondaryConfirmationAttachments()$"
              , secondaryConfirmationLink: "$_docviewSignatorySecondaryConfirmationLink()$"
            },
            showLinkDelivery : {
                title: "$_docviewShowLinkDeliveryModalTitle()$"
              , accept: "$_docviewShowLinkDeliveryModalAccept()$"
              , description: "$_docviewShowLinkDeliveryModalDescription()$"
            },
            composeLinkEmail : {
                compose: "$_docviewComposeLinkEmailCompose()$"
              , subject: "$_docviewComposeLinkEmailSubject()$"
              , body: "$_docviewComposeLinkEmailBody()$"
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
              , ssnFITupasLabel: "$_docviewChangeAuthenticationToViewModalFITupasSSNLabel()$"
              , ssnFITupasPlaceholder: "$_docviewChangeAuthenticationToViewModalFITupasPlaceholder()$"
              , phoneLabel: "$_docviewChangeAuthenticationToViewModalPhoneLabel()$"
              , flashMessageInvalidSESSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidSESSN()$"
              , flashMessageInvalidNOSSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidNOSSN()$"
              , flashMessageInvalidDKSSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidDKSSN()$"
              , flashMessageInvalidFISSN: "$_docviewChangeAuthenticationToViewFlashMessageInvalidFISSN()$"
              , flashMessageInvalidNOPhone: "$_docviewChangeAuthenticationToViewFlashMessageInvalidNOPhone()$"
              , flashMessageInvalidNOSSNAndPhone: "$_docviewChangeAuthenticationToViewFlashMessageInvalidNOSSNAndPhone()$"
              , errorFlashMessage: "$_docviewChangeAuthenticationToViewFlashMessageError()$"
            },
            changeAuthenticationToViewArchived : {
                title: "$_docviewChangeAuthenticationToViewArchivedModalTitle()$"
            },
            changeAuthentication : {
                title: "$_docviewChangeAuthenticationModalTitle()$"
              , accept: "$_docviewChangeAuthenticationModalAccept()$"
              , methodLabel: "$_docviewChangeAuthenticationModalMethodLabel()$"
              , placeholderSEEID: "$_docviewChangeAuthenticationModalPlaceholderSEEID()$"
              , placeholderNOEID: "$_docviewChangeAuthenticationModalPlaceholderNOEID()$"
              , placeholderDKEID: "$_docviewChangeAuthenticationModalPlaceholderDKEID()$"
              , placeholderFITupas: "$_docviewChangeAuthenticationModalPlaceholderFITupas()$"
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
           , payYearly: "$_paymentPayYearly()$"
           , payMonthly: "$_paymentPayMonthly()$"
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
                        , priceMonthly : {
                              EUR: "$TeamPriceMonthlyEURPerMonth()$"
                            , SEK: "$TeamPriceMonthlySEKPerMonth()$"
                        }
                        , users: "$_paymentPlanTeamUsers()$"
                        , allFromOnePlan: "$_paymentPlanTeamAllFromOnePlan()$"
                        , sla: "$_paymentPlanTeamSLA()$"
                        , salesforce: "$_paymentPlanTeamSalesforce()$"
                    }
                    , enterprise : {
                          name : "$EnterprisePlanName()$"
                        , priceOffer: "$_paymentPlanEnterprisePriceOffer()$"
                        , volumePricing: "$_paymentPlanEnterpriseVolumePricing()$"
                        , sla: "$_paymentPlanEnterpriseSLA()$"
                        , accountManager: "$_paymentPlanEnterpriseAccountManager()$"
                        , ipRestriction: "$_paymentPlanEnterpriseIPRestriction()$"
                        , customDevelopment: "$_paymentPlanEnterpriseCustomDevelopment()$"
                    }
                    , company : {
                          name : "$CompanyPlanName()$"
                        , price : {
                              EUR: "$CompanyPriceEURPerMonth()$"
                            , SEK: "$CompanyPriceSEKPerMonth()$"
                        }
                        , priceMonthly : {
                              EUR: "$CompanyPriceMonthlyEURPerMonth()$"
                            , SEK: "$CompanyPriceMonthlySEKPerMonth()$"
                        }
                        , users: "$_paymentPlanCompanyUsers()$"
                        , allFromTeamPlan: "$_paymentPlanCompanyAllFromTeamPlan()$"
                        , sla: "$_paymentPlanCompanySLA()$"
                        , branding: "$_paymentPlanCompanyBranding()$"
                        , accountManager: "$_paymentPlanCompanyAccountManager()$"
                    }
                    , one : {
                          name : "$OnePlanName()$"
                        , price : {
                              EUR: "$OnePriceEURPerMonth()$"
                            , SEK: "$OnePriceSEKPerMonth()$"
                        }
                        , users: "$_paymentPlanOneUsers()$"
                        , support: "$_paymentPlanOneSupport()$"
                        , evidencePackage: "$_paymentPlanOneEvidencePackage()$"
                        , basicBranding: "$_paymentPlanOneBasicBranding()$"
                        , billedAnnually: "$_paymentPlanOneBilledAnnually()$"
                        , languages: "$_paymentPlanOneLanguages()$"
                        , eid: "$_paymentPlanOneEID()$"
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
                    , prolong : {
                              action: "$_archiveProlong()$"
                            , body : "$_listLocalDialogsBodyProlong()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                            , invalidMessage : "$_flashMessageDocumentArchiveInvalidProlong()$"
                            , successMessage : "$_listprolongedflashmessagesuccess()$"
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
                    , dataRetentionPolicyRemark : "$_archiveDataRetentionPolicyRemark()$"
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
                    ,  unshare : {
                              action: "$_unshare()$"
                            , head : "$_listUnshareDialogHead()$"
                            , body : "$_listUnshareDialogBody()$"
                            , successMessage : "$_flashMessageTemplateUnshareDone()$"
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
                            , link: "$_templatesShareableLinkColumn()$"
                       }
                    , shareableLink: {
                        showTitle: "$_templatesShareableLinkShowTitle()$"
                      , showText: "$_templatesShareableLinkShowText()$"
                      , showTooltip: "$_templatesShareableLinkShowTooltip()$"
                      , generateTitle: "$_templatesShareableLinkGenerateTitle()$"
                      , generateText: "$_templatesShareableLinkGenerateText()$"
                      , generateTooltip: "$_templatesShareableLinkGenerateTooltip()$"
                      , regenerateText: "$_templatesShareableLinkRegenerateText()$"
                      , regenerateTooltip: "$_templatesShareableLinkRegenerateTooltip()$"
                      , generationSucceeded: "$_templatesShareableLinkGenerationSucceeded()$"
                      , generationFailed: "$_templatesShareableLinkGenerationFailed()$"
                      , discardTitle: "$_templatesShareableLinkDiscardTitle()$"
                      , discardText: "$_templatesShareableLinkDiscardText()$"
                      , discardTooltip: "$_templatesShareableLinkDiscardTooltip()$"
                      , discardButton: "$_templatesShareableLinkDiscardButton()$"
                      , discardingSucceeded: "$_templatesShareableLinkDiscardingSucceeded()$"
                      , discardingFailed: "$_templatesShareableLinkDiscardingFailed()$"
                      , copyButtonTooltip: "$_templatesShareableLinkCopyButtonTooltip()$"
                      , copySucceeded: "$_templatesShareableLinkCopySucceeded()$"
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
                    , subheadline30Days : "$_trashSubheadline30Days()$"
                    , subheadline24Hours : "$_trashSubheadline24Hours()$"
                    , search : "$_SearchDoc()$"
               }
        }
      , padlist : {
              updateAction : "$_padlistUpdateAction()$"
        }
      , loginModal : {
             login : "$_login()$"
           , email : "$_email()$"
           , password : "$_password()$"
           , twoFactor : "$_twoFactorInputLabel()$"
           , forgot : "$_forgot()$"
           , sendNewPassword : "$_loginModalForgotPasswordSendNewPassword()$"
           , loginFailed : "$_flashMessageLoginPageRedirectReasonWrongMailOrPassword()$"
           , loginFailedBadTOTP : "$_flashMessageWrongTwoFactorCode()$"
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
      }
      , apiDashboard : {
            reveal : "$_apiOauthReveal()$"
          , deleteToken :  "$_apiDashboardDelete()$"
          , grantedPriviligesDescription: "$_apiDashboardGrantedPriviligesDescription()$"
          , personalTokenDescription: "$_apiDashboardpersonalTokenDescription()$"
          , personalTokenCopyTooltip: "$_apiDashboardpersonalTokenCopyTooltip()$"
          , personalTokenCopySucceeded: "$_apiDashboardpersonalTokenCopySucceeded()$"
          , personalTokenWarning: "$_apiDashboardpersonalTokenWarning()$"
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
              , companynumber : "$_orgnumber()$"
              , companyposition : "$_companyposition()$"
              , companyaddress : "$_address()$"
              , companyzip :     "$_zip()$"
              , companycity :    "$_city()$"
              , companycountry : "$_country()$"
              , entityname     : "$_entityname()$"
              , entitynameExtraInfo: "$_entitynameExtraInfo()$"
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
              , userDeletionModalTitle : "$_userDeletionModalTitle()$"
              , generalError : "$_accountDetaulsGeneralError()$"
              , userDeletionModalMessage : {
                    line1 : "$_userDeletionModalMessageLine1()$"
                  , line2 : "$_userDeletionModalMessageLine2()$"
                  , line3 : "$_userDeletionModalMessageLine3()$"
                  , line4 : "$_userDeletionModalMessageLine4()$"
                  }
              , userDeletionModalConfirm : "$_userDeletionModalConfirm()$"
              , userNotDeletableDueToPendingDocuments : {
                    line1 : "$_userNotDeletableDueToPendingDocumentsLine1()$"
                  , line2 : "$_userNotDeletableDueToPendingDocumentsLine2()$"
                  }
              , deleteUser : "$_delete()$"
              , userDeleted : "$_userDeleted()$"
              , userGroup : "$_userGroup()$"
              , userGroupExtraInfo : "$_userGroupExtraInfo()$"
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
          , twoFactor : {
                twoFactorSection : "$_UserAccountSettingsTwoFactorSection()$"
              , twoFactorShortSection : "$_UserAccountSettingsTwoFactorShortSection()$"
              , disableFlashMessage : "$_UserAccountSettingsTwoFactorDisableFlashMessage()$"
              , setupSuccessFlashMessage : "$_UserAccountSettingsTwoFactorSetupSuccessFlashMessage()$"
              , setupIncorrectFlashMessage : "$_flashMessageWrongTwoFactorCode()$"
              , statusActive : "$_UserAccountSettingsTwoFactorStatusActive()$"
              , statusNotActive : "$_UserAccountSettingsTwoFactorStatusNotActive()$"
              , buttonActive : "$_UserAccountSettingsTwoFactorButtonActive()$"
              , buttonNotActive : "$_UserAccountSettingsTwoFactorButtonNotActive()$"
              , setupModalInstruction1 : "$_UserAccountSettingsTwoFactorSetupModalInstruction1()$"
              , setupModalInstruction2 : "$_UserAccountSettingsTwoFactorSetupModalInstruction2()$"
              , setupModalInstruction3 : "$_UserAccountSettingsTwoFactorSetupModalInstruction3()$"
              , setupModalInstruction4 : "$_UserAccountSettingsTwoFactorSetupModalInstruction4()$"
              , inputLabel : "$_twoFactorInputLabel()$"
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
              , columnCompany : "$_company()$"
              , columnRole :  "$_role()$"
              , editModal : {
                  editDetails : "$_modalEditUsersEditDetails()$"
                , save : "$_modalEditUsersSave()$"
                , change : "$_modalEditUsersChange()$"
                , changeEmail: "$_modalEditUsersChangeEmail()$"
                , requestSent:"$_modalEditUsersRequestSent()$"
                , emailInUse: "$_modalEditUsersEmailInUse()$"
                , settingsSaved: "$_modalEditUsersSettingsSaved()$"
              }
            }
          , stats : {
                name : "$_HeadUsageStats()$"
              , shareableLinksName: "$_HeadShareableLinksStats()$"
              , last30days : "$_last30Days()$"
              , last6months : "$_last6Months()$"
              , columnDate: "$_usageStatsDate()$"
              , columnSender : "$_statsSender()$"
              , columnTemplateTitle : "$_statsTemplateTitle()$"
              , columnClosedDocuments : "$_closedDocuments()$"
              , columnSendDocuments :"$_sentDocuments()$"
              , columnClosedSignatures : "$_closedSignatures()$"
              , openAsJson : "$_openAsJson()$"
            }
          , subscription : "$_Subscription()$"
          , dataRetention : {
                name : "$_dataRetentionName()$"
              , title : "$_dataRetentionTitle()$"
              , save : "$_save()$"
              , successMessage : "$_dataRetentionSuccessMessage()$"
              , sideBar : {
                    paragraph1 : "$_dataRetentionSideBarParagraph1()$"
                  , paragraph2 : "$_dataRetentionSideBarParagraph2()$"
                  , paragraph3 : "$_dataRetentionSideBarParagraph3()$"
                  , paragraph4 : "$_dataRetentionSideBarParagraph4()$"
                }
              , preparation : {
                    text : "$_dataRetentionPreparationText()$"
                  , description : "$_dataRetentionPreparationDescription()$"
                }
              , closed : {
                    text : "$_dataRetentionClosedText()$"
                  , description : "$_dataRetentionClosedDescription()$"
                }
              , canceled : {
                    text : "$_dataRetentionCanceledText()$"
                  , description : "$_dataRetentionCanceledDescription()$"
                }
              , timedout : {
                    text : "$_dataRetentionTimedoutText()$"
                  , description : "$_dataRetentionTimedoutDescription()$"
                }
              , rejected : {
                    text : "$_dataRetentionRejectedText()$"
                  , description : "$_dataRetentionRejectedDescription()$"
                }
              , error : {
                    text : "$_dataRetentionErrorText()$"
                  , description : "$_dataRetentionErrorDescription()$"
                }
              , immediateTrash : {
                    text : "$_dataRetentionImmediateTrashText()$"
                  , description : "$_dataRetentionImmediateTrashDescription()$"
                }
              , maximum : "$_dataRetentionMaximum()$"
              , companyMaximum : "$_dataRetentionCompanyMaximum()$"
            }
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
             , link         : "$_deliveryLink()$"
             , portal       : "Portal"
      }
      , signinginprogressmodal : {
               action1 : "$_signinginprogressmodalAction1()$"
             , action2 : "$_signinginprogressmodalAction2()$"
      }
      , signingInProgressDontCloseWindow : "$_signingInProgressDontCloseWindow()$"
      , signingStartedDontCloseWindow : "$_signingStartedDontCloseWindow()$"
      , signingErrorMessage1 : "$_signingErrorMessage1()$"
      , signingErrorMessage2 : "$_signingErrorMessage2()$"
      , signingErrorMessage3 : "$_signingErrorMessage3()$"
      , signingErrorReload   : "$_signingErrorReload()$"
      , integrations: {
              salesforce : "$salesforce()$"
            , salesforceDescription : "$salesforceDescription()$"
            , basicAuth : "$basicAuth()$"
            , basicAuthDescription : "$basicAuthDescription()$"
            , constantUrlCallback : "$constantUrlCallback()$"
            , constantUrlCallbackDescription : "$constantUrlCallbackDescription()$"
      }
      , header : {
              contact : "$_headerContakt()$"
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
      }
      , notNamedParty : "$_notNamedParty()$"
      , imageFileFormatNotSpportedIco : "$_imageFileFormatNotSpportedIco()$"
      , imageFileFormatNotSpported : "$_imageFileFormatNotSpported()$"
      , eID: {
          idName: {
              nemId: "$_eIdNameNemId()$"
          }
          , infoText: {
              nemId: "$_eIdInfoTextNemId()$"
          }
      }
    };
###
