####
#### This module puts in global context object 'localization'
#### If some script requires some string to be shown to the user
#### it just uses this module
####
javascriptLocalisation =
  window.localization =
        {      code : "$code$"
             , welcomeback : "$_welcomeback()$"
             , reloadPage : "$_reloadPage()$"
             , problemContactingServer : "$_problemContactingServer()$"
             , esigningpoweredbyscrive : "$PoweredByScrive()$"
             , sessionTimedoutInSignview : "$_sessionTimedoutInSignview()$"
             , sessionTimedoutInSignviewAfterHistoryBackMessage : "$_sessionTimedoutInSignviewAfterHistoryBackMessage()$"
             , sessionTimedoutInSignviewAfterHistoryBackTitle : "$_sessionTimedoutInSignviewAfterHistoryBackTitle()$"
             , signviewSigningFailed : "$_signviewSigningFailed()$"
             , resetYourPassword : "$_resetYourPassword()$"
             , resetYourPasswordCheckEmail : "$_resetYourPasswordCheckEmail()$"
             , getStartedInstantly : "$_getStartedInstantly()$"
             , freeDocumentPerMonth : "$_freeDocumentPerMonth()$"
             , signup : "$_signUp()$"
             , loadingFile : "$_loadingFile()$"
             , more : "$_more()$"
             , yourSigningPluginFailed : "$_yourSigningPluginFailed()$"
             , startingSaveSigning : "$_startingSaveSigning()$"
             , youCancelledSigning : "$_youCancelledSigning()$"
             , noTeliaInstalled  : "$_noTeliaInstalled()$"
             , noBankIdInstalled  : "$_noBankIdInstalled()$"
             , verifyingSignature : "$_verifyingSignature()$"
             , signviewAttachmentUploadedInOtherWindow : "$_signviewAttachmentUploadedInOtherWindow()$"
             , tbsMessage : "$_tbsYouSignWithElegHtml()$"
             , personalName : "$_fullName()$"
             , fstname : "$_firstname()$"
             , sndname : "$_lastname()$"
             , email : "$_email()$"
             , phone : "$_phone()$"
             , company : "$_company()$"
             , personalNumber : "$_personnumber()$"
             , companyNumber : "$_orgnumber()$"
             , next : "$_next()$"
             , statusToolTip: {
                   draft: "$_tooltipdraft()$"
                 , cancelled : "$_tooltipcancelled()$"
                 , sent : "$_tooltipsent()$"
                 , delivered : "$_tooltipdelivered()$"
                 , reviewed : "$_tooltipreviewed()$"
                 , read : "$_tooltipread()$"
                 , opened : "$_tooltipopened()$"
                 , signed : "$_tooltipsigned()$"
                 , rejected :  "$_tooltiprejected()$"
                 , deliveryproblem : "$_tooltipdeliveryproblem()$"
                 , timeouted       : "$_tooltiptimedout()$"
                 , problem           : "$_tooltiperror()$"
             }
             , listand : "$_listand()$"
             , you : "$_you()$"
             , searchBoxButtonText : "$_searchBoxButtonText()$"
             , downloadPDF : "$_DownloadPDF()$"
             , reviewAttachment: "$_ReviewAttachment()$"
             , reviewPDF: "$_ReviewPDF()$"
             , deletePDF: "$_DeletePDF()$"
             , attachmentType : "$_pageAttachmentAttachments()$"
             , requestedAttachments : "$_RequestedAttachments()$"
             , send : "$_send()$"
             , csvFilePersons : "$_csvFilePersons()$"
			 , reminder : {
					     send : "$_sendReminder()$"
					   , formHead : "$_reminderFormHead()$"
					   , formSend :	"$_reminderFormSendReminder()$"
                       , formOwnMessage : "$_reminderFormFooterOwnMessage()$"
                       , mobileQuestion : "$_reminderByMobileQuestion()$"
                       , mobileQuestionAlreadySigned : "$_reminderByMobileQuestionAlreadySigned()$"
                       , emailMobileQuestion : "$_reminderByEmailMobileQuestion()$"
					}
             , sign :  {
                          eleg : {
                                     subhead : "$_pageDocumentForSignContentElegSubhead()$"
                                   , body: "$_pageDocumentForSignContentElegBody()$ "
                                   , telia: "$_telia()$"
                                   , mobilebankid: "$_mobileBankID()$"
                                   , bankid: "$_bankid()$"
                                   , mobile : {
                                         startingMobileBankID : "$_bankidStartApp()$"
                                   }
                                   , failed : "$_signElegFailed()$"
                                   , mismatch : {
                                         mismatchOnNameAndNumberCantChange : "$_mismatchOnNameAndNumberCantChange()$"
                                       , mismatchOnNameAndNumberCanChange : "$_mismatchOnNameAndNumberCanChange()$"
                                       , mismatchOnNameAndCantChange : "$_mismatchOnNameAndCantChange()$"
                                       , mismatchOnNumberAndCantChange : "$_mismatchOnNumberAndCantChange()$"
                                       , mismatchOnNameAndCanChange : "$_mismatchOnNameAndCanChange()$"
                                       , mismatchOnNumberAndCanChange : "$_mismatchOnNumberAndCanChange()$"
                                   }
                          }
                    }
             , signByAuthor : {
                          modalTitle : "$_dialogConfirmSignByAuthorHead()$"

               }
             , reject : {
                         send : "$_pageDocumentForSignContentRejectFooterReject()$"
                       , editMessage : "$_pageDocumentForSignContentRejectFooterOwnMessage()$"
               }




             , signatoryMessage : {
                         datamismatch :"$_signatoryMessageIfSignatoryDatamismatch()$"
                       , timedout : "$_signatoryMessageIfTimedout()$"
                       , rejected : "$_signatoryMessageIfSignatoryRejected()$"
                       , seen : "$_signatoryMessageIfSignatorySeendate()$"
                       , read : "$_signatoryMessageIfSignatoryReaddate()$"
                       , delivered : "$_signatoryMessageIfSignatoryDeliveredEmail()$"
                       , signed : "$_signatoryMessageIfSigned()$"
                       , cancelled : "$_signatoryMessageIfCancelled()$"
                       , other : "$_signatoryMessageIfElse()$"
                       , waitingForSignature: "$_waitingForSignature()$"
                       , waiting : "$_signatoryMessageWaiting()$"
                    }
            , authorAttachmentBoxHeader : "$_signViewNotsignedbymeAttachmentBoxHead()$"
            , evidenceAttachmentBoxHeader : "$_evidenceAttachmentBoxHeader()$"
            , signatoryAttachmentUploadButton: "$_signViewSigAttachmentBoxChooseFile()$"
            , saveAsTemplate : "$_signStepsMenuEmptyStepSaveAsTemplate()$"
            , saveAsDraft    : "$_saveAsDraft()$"
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

            , selectFileToUpload : "$_selectFileToUpload()$"
            , onlyFiletypeAllowed : "$_onlyFiletypeAllowed()$"
            , fileTooLarge: "$_fileTooLarge()$"
            , couldNotUpload: "$_couldNotUpload()$"
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
               sendbuttontext: "$_contractsendbuttontext()$",
               changesignatorybuttontext: "$_changeSignatoryButtonText()$",
               startsigningbuttontext: "$_contractstartsigningbuttontext()$",
               confirmsendtitle: "$_contractconfirmsendtitle()$",
               startsigningtitle: "$_contractstartsigningtitle()$",
               restartbuttontext: "$_contractrestartbuttontext()$",
               prolongbuttontext: "$_contractprolongbuttontext()$",
               cancelbuttontext: "$_contractcancelbuttontext()$",
               cancelmodaltitle: "$_contractcancelmodaltitle()$",
               cancelmodaltext: "$_contractcancelmodaltext()$",
               cancelmodaltextwithsignatures: "$_contractcancelmodaltextwithsignatures()$",
               rejectbuttontext: "$_contractrejectbuttontext()$",
               signatorysignmodaltitlefromsignatorydrawing: "$_contractsignatorysignmodaltitlefromsignaturedrawing()$",
               signatorysignmodaltitleeleg: "$_contractsignatorysignmodaltitleeleg()$",
               signatorysignmodalcontent: "<p>$_contractsignatorysignmodalcontent()$ </p>",
               signatorysignmodalcontentfromsignaturedrawing: "<p>$_contractsignatorysignmodalcontentfromsignaturedrawing()$ </p>",
               signatorysignmodalcontentsignvieweleg: "<p>$_contractsignatorysignmodalcontentdesignvieweleg()$ </p>",
               signatorysignmodalcontentauthoronly: "<p>$_contractsignatorysignmodalcontentauthoronly()$ </p>",
               signbuttontext: "$_contractsignbuttontext()$",
               signbuttontextfromsignaturedrawing: "$_contractsignbuttontextfromsignaturedrawing()$",
               signatorycancelmodaltitle: "$_contractsignatorycancelmodaltitle()$",
               remindagainbuttontext: "$_contractremindagainbuttontext()$",
               authorname: "$_contractauthorname()$",
               authorsignatoryname: "$_contractauthorsignatoryname()$",
               signatoryname: "$_contractsignatoryname()$",
               nonsignatoryname: "$_contractnonsignatoryname()$",
               cancel: "$_cancel()$"
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
                , selectFromScrive : "$_updateAttachmentsDialogChooseFromScrive()$"
                , invalidAttachments : "$_invalidAttachments()$"
                , tooLargeAttachment : "$_tooLargeAttachment()$"
                , tooLargeAttachments : "$_tooLargeAttachments()$"
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
             , confirmSignature : "$_signatureConfirm()$"
             , clickToDraw : "$_clickToDraw()$"

           }
          , questionnaire : {
               heyImViktorFromScrive: "$_questionnaireHeyImViktorFromScrive()$"
             , ourEsignService: "$_questionnaireOurEsignService()$"
             , quickQuestion: "$_questionnaireQuickQuestion()$"
             , enjoySigning: "$_questionnaireEnjoySigning()$"
             , yes: "$_questionnaireYes()$"
             , no: "$_questionnaireNo()$"
             , great: "$_questionnaireGreat()$"
             , sendDownP: "$_questionnaireSendDownP()$"
             , sendDown2: "$_questionnaireSendDown2()$"
             , closeFaster: "$_questionnaireCloseFaster()$"
             , showDemo: "$_questionnaireShowDemo()$"
             , seeHowSimple: "$_questionnaireSeeHowSimple()$"
             , easyToSign: "$_questionnaireEasyToSign()$"
             , otherBenefitsP: "$_questionnaireOtherBenefitsP()$"
             , thanksForYourTime: "$_questionnaireThanksForYourTime()$"
             , haveAGreatDay: "$_questionnaireHaveAGreatDay()$"
             , sorry: "$_questionnaireSorry()$"
             , improvements: "$_questionnaireImprovements()$"
             , sendFeedback: "$_questionnaireSendFeedback()$"
             , lastQuestion: "$_questionnaireLastQuestion()$"
             , anyoneElse: "$_questionnaireAnyoneElse()$"
             , phoneNumber: "$_questionnairePhoneNumber()$"
         }
         , ftue : {
               step1: "$_FTUEstep1()$"
             , step2: "$_FTUEstep2()$"
             , step3: "$_FTUEstep3()$"
             , step4: "$_FTUEstep4()$"
             , step1description: "$_FTUEstep1description()$"
             , step2description: "$_FTUEstep2description()$"
             , step3description: "$_FTUEstep3description()$"
             , step3descriptionScriver: "$_FTUEstep3descriptionScriver()$"
             , step3descriptionYourself: "$_FTUEstep3descriptionYourself()$"
             , step4description: "$_FTUEstep4description()$"
             , useSampleDocument: "$_FTUEuseSampleDocument()$"
             , addYourself: "$_FTUEaddYourself()$"
             , addAScriver: "$_FTUEaddAScriver()$"
             , documentTitle: "$_documentTitle()$"
           }
         , phonePlaceholder: "$_phonePlaceholder()$"
         , designview : {
              editMessagePlaceholder: "$_editMessagePlaceholder()$",
              chooseParticipant: "$_designViewChooseParticipant()$",
              selectField: "$_selectField()$",
              validation : {
                      missingOrWrongEmail : "$_emptyOrNotValidEmail()$"
                    , missingOrWrongPersonalNumber : "$_backToStepTwoAndFillPersonNumber()$"
                    , missingOrWrongMobile : "$_emptyOrNotValidMobile()$"
                    , missingOrWrongPlacedAuthorField : "$_missingPlacedAuthorFieldValue()$"
                    , notReadyField : "$_setAllFields()$"
                    , pleaseSelectField : "$_validationPleaseSelectField()$"
                    , requestAttachmentFlashMessage : "$_designViewAttachmentRequestedFlashMessage()$"
                }
             , signviewsettings : {
                  showheader: "$_signviewSettingsShowHeader()$"
                , showpdfdownload: "$_signviewSettingsShowPDFDownload()$"
                , showrejectoption: "$_signviewSettingsShowRejectOption()$"
                , showfooter: "$_signviewSettingsShowFooter()$"
                , title: "$_signviewSettingsTitle()$"
                , button: "$_signviewSettingsButton()$"
               }
             , sign : "$_sign()$"
             , textFields : {
                      obligatory: "$_textFieldObligatory()$"
                    , forThis : "$_textFieldFor()$"
                    , done : "$_textFieldDone()$"
             }
             , checkboxes : {
                      checkbox : "$_checkboxCheckbox()$"
                    , prechecked : "$_checkboxPrechecked()$"
                    , obligatory: "$_checkboxObligatory()$"
                    , forThis : "$_checkboxFor()$"
                    , done : "$_checkboxDone()$"
             }
             , cantSignModal : {
                 title : "$_designviewCantSignModalTitle()$"
               , info : "$_designviewCantSignModalInfo()$"
               , emailFormatInfo : "$_designviewCantSignModalEmailFormatInfo()$"
               , emailSample : "$_designviewCantSignModalEmailSample()$"
               , mobileFormatInfo : "$_designviewCantSignModalMobileFormatInfo()$"
               , mobileSample : "$_designviewCantSignModalMobileSample()$"
             }
             , editParticipants: "$_designViewEditParticipants()$"
             , editDocument: "$_designViewEditDocument()$"
             , optionalField: "$_designViewOptionalField()$"
             , editSigningProcess: "$_designViewEditSigningProcess()$"
             , startSigning: "$_designViewStartSigning()$"
             , addField: "$_designViewAddField()$"
             , whatField: "$_designViewWhatField()$"
             , customField: "$_designViewCustomField()$"
             , fieldName: "$_designViewFieldName()$"
             , defineParticipation: "$_designViewDefineParticipation()$"
             , toReceiveDocument: "$_designViewToReceiveDocument()$"
             , byEmail: "$_designViewByEmail()$"
             , byAPI: "$_designViewByAPI()$"
             , onThisTablet: "$_designViewOnThisTablet()$"
             , bySMS: "$_designViewBySMS()$"
             , byEmailAndSMS : "$_designViewbyEmailAndSMS()$"
             , forSigning: "$_designViewForSigning()$"
             , forViewing: "$_designViewForViewing()$"
             , noIDControl: "$_designViewNoIDControl()$"
             , withEleg: "$_designViewWithEleg()$"
             , withSMSPin: "$_designViewWithSMSPin()$"
             , withEmailConfirmation: "$_withEmailConfirmationg()$"
             , withMobileConfirmation: "$_withMobileConfirmation()$"
             , withEmailMobileConfirmation: "$_withEmailMobileConfirmation()$"
             , withoutConfirmation: "$_withoutConfirmation()$"
             , addParty: "$_designViewAddParty()$"
             , addParties : {
                 close : "$_designViewAddPartiesClose()$"
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
             , signatureBoxSettings: "$_designViewSignatureBoxSettings()$"
             , freeTextBox: "$_designViewFreeTextBox()$"
             , checkbox: "$_checkBoxFieldName()$"
             , removeThisDocument: "$_designViewRemoveThisDocument()$"
             , attached : "$_designViewAttached()$"
             , attachmentRequestedFrom : "$_designViewAttachmentRequestedFrom()$"
             , requestFieldFrom : "$_designViewRequestFieldFrom()$"
             , draggablehelp1 : "$_designviewdraggablehelp1()$"
             , draggablehelp2 : "$_designviewdraggablehelp2()$"
             , days :"$_designviewdays()$"
             , saved : "$_designviewsaved()$"
             , fromCSV : "$_fromCSV()$"
             , numPartOfNum : "$_numPartOfNum()$"
             , preparingDocument : "$_preparingDocument()$"
             , fieldWithSameNameExists : "$_fieldWithSameNameExists()$"
             , emailCanBeChangedInAccountSection : "$_emailCanBeChangedInAccountSection()$"
             , nameCanBeChangedInAccountSection : "$_nameCanBeChangedInAccountSection()$"
             , customMessage : {
                 invitation   : "$_designViewCustomMessageInvitation()$"
               , confirmation : "$_designViewCustomMessageConfirmation()$"
               , preview      : "$_designViewCustomMessagePreview()$"
             }
             , cantRemoveAllIdentificationFieldsText : "$_cantRemoveAllIdentificationFieldsText()$"
             , cantMakeAllIdentificationFieldsOptionalText : "$_cantMakeAllIdentificationFieldsOptionalText()$"
             , information : "$_information()$"
             , signModalContentAuthorOnly : "<p>$_designViewSignModalContentAuthorOnly()$</p>"
             , signModalContentEleg : "<p>$_designViewSignModalContentEleg()$</p>"
             , signModalContent : "<p>$_designViewSignModalContent()$</p>"
             , startSigningModalTextWithPad : "$_designViewStartSigningModalTextWithPad()$"
             , startSigningModalText : "$_designViewSigningModalText()$"
             , startSigningModalTextWithSendout : "$_designViewStartSigningModalTextWithSendout()$"
         }
         , validation : {
                required: "$_nameRequired()$"
              , toolong: "$_nameTooLong()$"
              , invalidnamechars: "$_nameInvalidChars()$"
              , firstNameField: "$_firstNameField()$"
              , lastNameField: "$_lastNameField()$"
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
                drawSignatureBoxHeader : "$_padDrawSignatureBoxHeader()$"
             ,  typeSignatureBoxHeader : "$_padTypeSignatureBoxHeader()$"
             ,  typeSignatureNameField : "$_padTypeSignatureNameField()$"
             ,  fillInExtraDetails : "$_padFillInExtraDetails()$"
             ,  cleanImage : "$_padCleanImage()$"
             ,  notNamedParty : "$_notNamedParty()$"
             ,  changePartyTo : "$_changePartyTo()$"
             ,  giveForSigningThisDevice : "$_padGiveForSigningThisDevice()$"
             ,  font1 : "$_padFont1()$"
             ,  font2 : "$_padFont2()$"
             ,  font3 : "$_padFont3()$"
             ,  backToList : "$_padBackToList()$"
             ,  backToDocument : "$_padBackToDocument()$"
        }
         , changeEmail : "$_changeEmail()$"
         , changePhone : "$_changePhone()$"
         , accessNewAccountModal: {
             header: "$_accessNewAccountHeader()$"
           , body: "$_accessNewAccountBody()$"
           , button: "$_accessNewAccountButton()$"
         }
         , newPasswordModal: {
             modalNewPasswordViewNewPassword: "$_modalNewPasswordViewNewPassword()$"
           , modalNewPasswordViewRepeatPassword: "$_modalNewPasswordViewRepeatPassword()$"
           , modalNewPasswordViewHeader: "$_modalNewPasswordViewHeader()$"
           , modalNewPasswordViewFooterSave: "$_modalNewPasswordViewFooterSave()$"
           , flashMessagePasswordsDontMatch: "$_flashMessagePasswordsDontMatch()$"
           , flashMessageMissingRequiredField: "$_flashMessageMissingRequiredField()$"
           , flashMessagePasswordExceedsMaxLength: "$_flashMessagePasswordExceedsMaxLength()$"
           , flashMessageInvalidCharsInPassword: "$_flashMessageInvalidCharsInPassword()$"
           , flashMessagePasswordInvalid: "$_flashMessagePasswordInvalid()$"
           , flashMessagePasswordChangeLinkNotValid: "$_flashMessagePasswordChangeLinkNotValid()$"
         }
         , signupModal: {
             modalAccountSetupFooter: "$_modalAccountSetupFooter()$"
           , alreadyHaveAnAccount : "$_alreadyHaveAccount()$"
         }
         , visitOurPricingPage : "$_visitOurPricingPage()$"
         , accountSetupModal: {
             modalAccountSetupTOS: "$_modalAccountSetupTOS()$"
           , modalAccountSetupChoosePassword: "$_modalAccountSetupChoosePassword()$"
           , modalAccountSetupRepeatPassword: "$_modalAccountSetupRepeatPassword()$"
           , modalAccountSetupPhone: "$_modalAccountSetupPhone()$"
           , modalAccountSetupPosition : "$_modalAccountSetupPosition()$"
           , modalAccountSetupCompany : "$_modalAccountSetupCompany()$"
           , modalAccountPasswordRequirements : "$_modalAccountPasswordRequirements()$"
           , flashMessageUserAlreadyActivated : "$_flashMessageUserAlreadyActivated()$"
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
             , firsttime : {
                   emailDelivered : "$InvitationDeliveredByEmailText(signatory='ACTOR';signing='TRUE')$"
                 , emailRead : "$MarkInvitationReadEvidenceText(signatory_email='EMAIL';signing='TRUE')$"
                 , tryYourself : "$_authorviewFTUETryYourself()$"
                 , linkOpened : "$SignatoryLinkVisitedText(actor='ACTOR')$"
                 , documentSigned : "$SignDocumentEvidenceText(actor='ACTOR')$"
                 , documentSealed : "$AttachGuardtimeSealedFileEvidenceText()$"
                 , branding: "$_authorviewFTUEBrand()$"
                 , contact: "$_authorviewFTUEContact()$"
                 , contactdetails: "$_authorviewFTUEContactDetails()$"
                 , upload: "$_authorviewFTUEUpload()$"
                 , docsent: "$_authorviewFTUEDocSent()$"
                 , dochistory: "$_authorviewFTUEDocHistory()$"
             }
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
             , subtitleSignedClosedByEmail: "$_subtitleSignedClosedByEmail()$"
             , subtitleSignedClosedByMobile: "$_subtitleSignedClosedByMobile()$"
             , subtitleSignedClosedByEmailMobile: "$_subtitleSignedClosedByEmailMobile()$"
             , subtitleSignedNotClosedByEmail: "$_subtitleSignedNotClosedByEmail()$"
             , subtitleSignedNotClosedByMobile: "$_subtitleSignedNotClosedByMobile()$"
             , subtitleSignedNotClosedByEmailMobile: "$_subtitleSignedNotClosedByEmailMobile()$"
             , unavailableForSign: "$_unavailableForSign()$"
             , dueDate: "$_dueDate()$"
             , companyNumberLabel: "$_companyNumberLabel()$"
             , personalNumberLabel: "$_personalNumberLabel()$"
             , authorAttachmentsTitleForOne: "$_signViewAuthorAttachmentsTitleForOne()$"
             , authorAttachmentsTitleForLots: "$_signViewAuthorAttachmentsTitleForLots()$"
             , signatoryAttachmentsTitleForOne: "$_signViewSignatoryAttachmentsTitleForOne()$"
             , signatoryAttachmentsTitleForLots: "$_signViewSignatoryAttachmentsTitleForLots()$"
             , signatoryAttachmentsSupportedFormats : "$_signatoryAttachmentsSupportedFormats()$"
             , signatoriesTitle: "$_signViewSignatoriesTitle()$"
             , acceptTOS: "$_acceptTOS()$"
             , notEntered: "$_notEntered()$"
             , checkbox: "$_signViewCheckbox()$"
             , signature: "$_signViewSignature()$"
             , textfield: "$_signViewTextField()$"
             , filladitionfields : "$_filladitionfields()$"
             , filladitionfieldsdescription : "$_filladitionfieldsdescription()$"
             , additionalFieldsMissingInModal : "$_filladitionfieldsdescription()$"
             , mandatoryAction: "$_mandatoryAction()$"
             , optionalAction: "$_optionalAction()$"
             , promoBannerButton: "$_promoBannerButton()$"
             , signupButtonText: "$_signupButtonText()$"
             , titleText: "$_titleText()$"
             , subtitleText: "$_subtitleText()$"
             , pinSigning : {
                  invalidPhone   :  "$_invalidPhone()$"
                , signWithSMSPin :  "$_signWithSMSPin()$"
                , noPinProvided   : "$_noPinProvided()$"
                , invalidPin      : "$_invalidPin()$"
                , enterSMSPin     : "$_enterSMSPin()$"
                , pinWillBeSentTo : "$_pinWillBeSentTo()$"
                , enterPhoneForPinDelivery : "$_enterPhoneForPinDelivery()$"
                , next : "$_next()$"
                , checkYourPhone : "$_checkYourPhone()$"

             }
        }
        , docview : {
            signatory : {
                editAuthenticationMethod: "$_docviewSignatoryeditAuthenticationMethod()$"
              , invitationOrder: "$_docviewSignatoryInvitationOrderLabel()$"
              , invitationMethod: "$_docviewSignatoryInvitationMethodLabel()$"
              , invitationEmail: "$_docviewSignatoryInvitationEmail()$"
              , invitationPad: "$_docviewSignatoryInvitationPad()$"
              , invitationSMS: "$_docviewSignatoryInvitationSMS()$"
              , invitationEmailSMS: "$_docviewSignatoryInvitationEmailSMS()$"
              , invitationAPI: "$_docviewSignatoryInvitationAPI()$"
              , role: "$_docviewSignatoryRoleLabel()$"
              , roleSignatory: "$_docviewSignatoryRoleSignatory()$"
              , roleViewer: "$_docviewSignatoryRoleViewer()$"
              , authentication: "$_docviewSignatoryAuthenticationMethodLabel()$"
              , authenticationStandard: "$_docviewSignatoryAuthenticationStandard()$"
              , authenticationSMSPin: "$_docviewSignatoryAuthenticationSMSPin()$"
              , authenticationELeg: "$_docviewSignatoryAuthenticationELeg()$"
              , confirmation: "$_docviewSignatoryConfirmationMethodLabel()$"
              , confirmationEmail: "$_docviewSignatoryConfirmationEmail()$"
              , confirmationSMS: "$_docviewSignatoryConfirmationSMS()$"
              , confirmationEmailSMS: "$_docviewSignatoryConfirmationEmailSMS()$"
              , confirmationNone: "$_docviewSignatoryConfirmationNone()$"
            },
            changeAuthentication : {
                title: "$_docviewChangeAuthenticationModalTitle()$"
              , accept: "$_docviewChangeAuthenticationModalAccept()$"
              , methodLabel: "$_docviewChangeAuthenticationModalMethodLabel()$"
              , placeholderEID: "$_docviewChangeAuthenticationModalPlaceholderEID()$"
              , placeholderPhone: "$_docviewChangeAuthenticationModalPlaceholderPhone()$"
              , valueInfotext: "$_docviewChangeAuthenticationModalValueInfotext()$"
              , errorSigned: "$_docviewChangeAuthenticationModalErrorSigned()$"
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
             users: "$_paymentsUsers()$"
           , user: "$_paymentsUser()$"
           , vat: "$_paymentsVat()$"
           , vat25: "$_paymentsVat25()$"
           , expires: "$_paymentsExpires()$"
           , creditcardnumber: "$_paymentsCreditCardNumber()$"
           , contact: "$_paymentPlanContact()$"
           , already: "$_paymentPlanAlreadySubscribed()$"
           , alreadyGo: "$_paymentPlanAlreadySubscribedGo()$"
           , placeholder : "$_paymentPlanPlaceholder()$"
           , sendmsg : "$_paymentPlanSendMessage()$"
	   , numberOfUsers: "$_paymentNumberOfUsers()$"
           , plans: {
                      team         : {
                      name : "$_paymentPlanTeam()$",
                      tag :"$_paymentPlanTeamTag()$",
                      price : "$_paymentsTeamPrice()$",
                      price3 : "$_paymentPlanPerUserPerMonth()$"
                    }
                    , form : {
                      name : "$_paymentPlanForm()$",
                      tag : "$_paymentPlanFormTag()$",
                      price : "$_paymentPlanFormPrice()$",
                      price3 : "$_paymentPlanPerMonthPerDoc()$"
                    }
                    , enterprise : {
                      name : "$_paymentPlanEnterprise()$",
                      tag : "$_paymentPlanEnterpriseTag()$",
                      price : "$_paymentsQuote()$",
                      price3 : "$_paymentsGetInTouch()$"
                    }
                    , trial : {
                      name : "$_paymentPlanTrial()$",
                      tag : "$_paymentPlanTrialTag()$",
                      price : "$_paymentsQuote()$",
                      price3 : "$_paymentsGetInTouch()$"
                    }
            }
           , table: { currentplan : "$_paymentPlanCurrentPlan()$"
                    , total       : "$_paymentPlanTotal()$"
                    , payments    : "$_paymentPlanPayments()$"
                    , nextpayment : "$_paymentPlanNextPayment()$"
                    , previouspayments : "$_paymentPlanPreviousPayments()$"
                    , changebilling: "$_paymentPlanChangeBilling()$"
                    , none: "$_paymentPlanNone()$"
                    }
            , subscribe: "$_paymentPlanSubscribe()$"
            , purchase: "$_paymentPlanPurchase()$"
            , subscribeheader : "$_paymentsSubscribeHeader()$"
            , savechanges : "$_SaveChanges()$"
            , permonth: "$_paymentPlanPerMonth()$"
            , savingsubscription : "$_paymentPlanSavingSubscription()$"
            , loading : "$_paymentPlanLoading()$"
            , askviktor : "$_paymentPlanAskViktor()$"
            , cancelsubscription: "$_paymentPlanCancelSubscription()$"
            , errors: { emptyField: "$_paymentsErrorsEmptyField()$"
                      , missingFullAddress: "$_paymentsErrorsMissingFullAddress()$"
                      , invalidEmail: "$_paymentsErrorsInvalid()$"
                      , invalidCC: "$_paymentsErrorsInvalid()$"
                      , invalidCVV: "$_paymentsErrorsInvalid()$"
                      , invalidCoupon: "$_paymentsErrorsInvalid()$"
                      , cardDeclined: "$_paymentsErrorsCardDeclined()$"
                      , acceptTOS: "$_paymentsErrorsAcceptTOS()$"
                      , invalidQuantity: "$_paymentsErrorsInvalidQuantity()$"
                      }
              , outside: {
                  confirmAccountCreatedUser       : "$_confirmAccountCreatedUser()$",
                  confirmAccountCreatedUserHeader : "$_modalUserSignupDoneHeader()$",
                  confirmAccountExistingUser      : "$_confirmAccountExistingUser()$",
                  confirmAccountExistingUserHeader: "$_confirmAccountExistingUserHeader()$",
                  sorryExistingUser               : "$_flashMessageUserWithSameEmailExists()$",
                  sorryExistingUserHeader         : "$_mailDocumentErrorContentBodyFourth()$"
              }
              , cancelDialog : "$_paymentsCancelDialog()$"
              , cancelingSubscription : "$_paymentsCancelingSubscription()$"
              , renewingSubscription : "$_paymentsRenewingSubscription()$"
              , renewSubscription : "$_paymentsRenewSubscription()$"
              , renewDialog : "$_paymentsRenewDialog()$"
              , billingInfoSaved : "$_paymentsBillingInfoSaved()$"
              , wasCanceled : "$_paymentsWasCanceled()$"
              , canceledDate : "$_paymentsCanceledDate()$"
              , contactAdmin : "$_paymentsContactAdmin()$"
              , mustBeAdmin : "$_paymentsMustBeAdmin()$"
              , subscriptionRenewed : "$_paymentsSubscriptionRenewed()$"
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
                              action: "$_SendReminder()$"
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
                }
            , ftue: {
                       title: "$_title()$"
                     , subtitleInJustAFewMinutes: "$_subtitleInJustAFewMinutes()$"
                     , subtitleIsntThisHowSimple: "$_subtitleIsntThisHowSimple()$"
                     , subtitleReturned: "$_subtitleReturned()$"
                     , subtitleReviewed: "$_subtitleReviewed()$"
                     , subtitleSaved: "$_subtitleSaved()$"
                     , button: "$_button()$"
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
                            , shared      : "$_shared()$"
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
      , login : "$_login()$"
      , loginModal : {
             login : "$_login()$"
           , email : "$_email()$"
           , password : "$_password()$"
           , forgotpassword : "$_forgotpassword()$"
           , sendNewPassword : "$_loginModalForgotPasswordSendNewPassword()$"
           , loginFailed : "$_flashMessageLoginPageRedirectReasonWrongMailOrPassword()$"
           , loginFailedBadIP : "$_flashMessageLoginPageRedirectReasonWrongIPAddress()$"
           , invalidEmail : "$_flashEmailIncorectlyFormed()$"
           , noUser : "$_cantSendReminderNoUser()$"
           , tooMuch : "$_cantSendReminderTooMuch()$"
           , passwordReminderSend : "$_flashMessageChangePasswordEmailSend()$"
           , dontHaveAccount : "$_dontHaveAccount()$"
      }
      , blocking : {
          free: {
            create : {
              title  : "$_blockingFreeCreateTitle()$"
            },
            csv : {
              header : "$_blockingFreeCSVHeader()$"
            },
            click: {
              title : "$_blockingFreeClickTitle()$"
            },
            has : {
              headline : "$_blockingFreeHasHeadline()$"
            },
            hasNot : {
              headline : "$_blockingFreeHasNotHeadline()$",
              subtext1 : "$_blockingFreeHasNotSubtext1()$"
            },
            heading : "$_blockingFreeHeading()$"
          },
          overdue : {
            create : {
              title : "$_blockingOverdueCreateTitle()$",
              body  : "$_blockingOverdueCreateBody()$"
            },
            csv : {
              body  : "$_blockingOverdueCSVBody()$"
            },
            headline : "$_blockingOverdueHeadline()$",
            subtext1 : "$_blockingOverdueSubtext1()$",
            subtext2 : "$_blockingOverdueSubtext2()$"
          },
          dunning : {
            headline : "$_blockingDunningHeadline()$",
            subtext1 : "$_blockingDunningSubtext1()$",
            subtext2 : "$_blockingDunningSubtext2()$"
          },
          canceled : {
            create : {
              title : "$_blockingCanceledCreateTitle()$",
              body  : "$_blockingCanceledCreateBody()$"
            },
            csv : {
              body  : "$_blockingCanceledCSVBody()$"
            },
            headline : "$_blockingCanceledHeadline()$",
            subtext1 : "$_blockingCanceledSubtext1()$",
            subtext2 : "$_blockingCanceledSubtext2()$"
          },
          deactivated : {
            create : {
              title : "$_blockingDeactivatedCreateTitle()$",
              body  : "$_blockingDeactivatedCreateBody()$"
            },
            csv : {
              body  : "$_blockingDeactivatedCSVBody()$"
            },
            headline : "$_blockingDeactivatedHeadline()$",
            subtext1 : "$_blockingDeactivatedSubtext1()$"
          },
          willcancel : {
            headline : "$_blockingWillCancelHeadline()$",
            subtext1 : "$_blockingWillCancelSubtext1()$"
          },
          usedall : {
            headline : "$_blockingUsedAllHeadline()$",
            subtext1 : "$_blockingUsedAllSubtext1()$"
          },
          paying : {
            create : {
              title : "$_blockingPayingCreateTitle()$",
              body  : "$_blockingPayingCreateBody()$"
            },
            csv : {
              body  : "$_blockingPayingCSVBody()$"
            }
          },
           button : {
             doublecheck : "$_blockingButtonDoubleCheck()$",
             reinstate   : "$_blockingButtonReinstate()$",
             contact     : "$_blockingButtonContact()$"
           }
        }
      , apiConfiration : {
             title : "$_apiConfiramtionTitle()$"
           , header : "$_apiConfiramtionHeader()$"
           , footerNotLogged : "$_apiConfiramtionFooterNotLogged()$"
           , footerLogged : "$_apiConfiramtionFooterLogged()$"
           , readPermission : "$_apiConfiramtionReadPermission()$"
           , createPermission : "$_apiConfiramtionCreatePersmission()$"
           , sendPermission : "$_apiConfiramtionSendPermission()$"
           , accept : "$_apiConfiramtionAccept()$"
           , loginAndAcceptTitle : "$_apiConfiramtionLoginAndAcceptTitle()$"
           , loginAndAccept : "$_apiConfiramtionLoginAndAccept()$"
           , createAccount : "$_apiConfiramtionCreateAccount()$"
           , enterEmailAdress : "$_apiConfiramtionEnterEmailAdress()$"
           , deny : "$_apiConfiramtionDeny()$"
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
              , smsOriginator : "$_smsOriginator()$"
              , smsOriginatorDescription : "$_smsOriginatorDescription()$"
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
      , sampleServiceView : {
            headerSend: "$_sampleServiceViewHeaderSend()$"
          , headerTemplate: "$_sampleServiceViewHeaderTemplate()$"
          , headerArchive: "$_sampleServiceViewHeaderArchive()$"
          , headerAccount: "$_sampleServiceViewHeaderAccount()$"
          , headerLogOut: "$_sampleServiceViewHeaderLogOut()$"
      }
      , sampleSignView : {
            mandatoryAction: "$_sampleSignViewMandatoryAction()$"
          , optionalAction: "$_sampleSignViewOptionalAction()$"
          , followTheArrow: "$_sampleSignViewFollowTheArrow()$"
          , phone : "$_sampleSignViewPhone()$"
          , email : "$_sampleSignViewEmail()$"
          , textField : "$_sampleSignViewTextField()$"
          , reviewPDF : "$_sampleSignViewReviewPDF()$"
          , rejectButton : "$_sampleSignViewRejectButton()$"
          , signButton : "$_sampleSignViewSignButton()$"
          , attachmentsTitle : "$_sampleSignViewAttachmentsTitle()$"
          , attachmentFilename : "$_sampleSignViewAttachmentFilename()$"
          , demoDocumentName : "$_sampleSignViewDemoDocumentName()$"
          , signViewHeader : "$_sampleSignViewHeader()$"
      }
      , companyBranding : {
               customiseFontLabel : "$_customiseFontLabel()$"
             , customiseSignViewBackgroundColour : "$_customiseSignViewBackgroundColour()$"
             , signviewBranding : "$_signviewBranding()$"
             , serviceBranding : "$_serviceBranding()$"
             , customiseBorderColourLabel : "$_customiseBorderColourLabel()$"
             , customiseButtonColourLabel : "$_customiseButtonColourLabel()$"
             , customiseEmailBackgroundColour : "$_customiseEmailBackgroundColourLabel()$"
             , primaryColour: "$_companyBrandingPrimaryColour()$"
             , primaryTextColour: "$_companyBrandingPrimaryTextColour()$"
             , secondaryColour: "$_companyBrandingSecondaryColour()$"
             , secondaryTextColour: "$_companyBrandingSecondaryTextColour()$"
             , barsColour : "$_companyBrandingBarsColour()$"
             , barsTextColour : "$_companyBrandingBarsTextColour()$"
             , barsSecondaryColour : "$_companyBrandingbarsSecondaryColour()$"
             , saveBranding: "$_SaveSettings()$"
             , emailBranding: "$_EmailBranding()$"
             , customiseLogo: "$_CustomiseLogo()$"
             , customiseBackgroundColour: "$_CustomiseBackgroundColour()$"
             , customiseTextColour: "$_CustomiseTextColour()$"
             , selectImageLabel: "$_SelectImageLabel()$"
             , customize : "$_companyBrandingCustomize()$"
             , brandingPreview: {
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
               title : "$_signinginprogressmodalTitle()$"
             , action1 : "$_signinginprogressmodalAction1()$"
             , action2 : "$_signinginprogressmodalAction2()$"
             , youHaveSignedPleaseWait : "$_signinginprogressmodalYouHaveSignedPleaseWait()$"
      }
      , signingInProgressDontCloseWindow : "$_signingInProgressDontCloseWindow()$"
      , signingErrorMessage1 : "$_signingErrorMessage1()$"
      , signingErrorMessage2 : "$_signingErrorMessage2()$"
      , signingErrorReload   : "$_signingErrorReload()$"
      , integrations: {
              salesforce : "$salesforce()$"
            , salesforceDescription : "$salesforceDescription()$"
            , constantUrlCallback : "$constantUrlCallback()$"
            , constantUrlCallbackDescription : "$constantUrlCallbackDescription()$"
      }
      , footer : {
              explore              : "$_pageFooterExplore()$"
            , api                  : "$_pageFooterAPI()$"
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
            , address2             : "$_pageFooterAddress2()$"
            , address3             : "$_pageFooterAddress3()$"
            , address4             : "$_pageFooterAddress4()$"
      }
      , header : {
              login : "$_login()$"
            , startFreeTrial : "$_startFreeTrial()$"
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
    };
###
