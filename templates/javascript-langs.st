####
#### This module puts in global context object 'localization'
#### If some script requires some string to be shown to the user
#### it just uses this module
####
javascriptLocalisation =
  window.localization =
        {      code : "$_code()$"
             , welcomeback : "$_welcomeback()$"
             , esigningpoweredbyscrive : "E-signing powered by Scrive"
             , resetYourPassword : "$_resetYourPassword()$"
             , resetYourPasswordCheckEmail : "$_resetYourPasswordCheckEmail()$"
             , getStartedInstantly : "$_getStartedInstantly()$"
             , freeDocumentPerMonth : "$_freeDocumentPerMonth()$"
             , signup : "$_signUp()$"
             , loadingFile : "$_loadingFile()$"
             , more : "$_more()$"
             , yourSigningPluginFailed : "$_yourSigningPluginFailed()$"
             , startingSaveSigning : "$_startingSaveSigning()$"
             , noNordeaInstalled  : "$_noNordeaInstalled()$"
             , noTeliaInstalled  : "$_noTeliaInstalled()$"
             , noBankIdInstalled  : "$_noBankIdInstalled()$"
             , verifyingSignature : "$_verifyingSignature()$"
             , tbsGenerationMessage : function(doctitle, docid){
                    return  '$_tbsGenerationMessage1()$"' + doctitle + '"$_tbsGenerationMessage2()$"' + docid + '"$_tbsGenerationMessage3()$'
                     }
             , ie6NotSupported : "$_ie6NotSupported()$"
             , name : "$_name()$"
             , fstname : "$_firstname()$"
             , sndname : "$_lastname()$"
             , email : "$_email()$"
             , phone : "$_phone()$"
             , company : "$_company()$"
             , personamNumber : "$_personnumber()$"
             , companyNumber : "$_orgnumber()$"
             , fieldName : "$_fieldName()$"
             , noNamePerson : "$_noNamePerson()$"
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
             , and : "$_and()$"
             , listand : "$_listand()$"
             , you : "$_you()$"
             , searchBoxButtonText : "$_searchBoxButtonText()$"
             , writeHere : "$_writeHere()$"
             , checkHere : "$_checkHere()$"
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
                       , emailMobileQuestion : "$_reminderByEmailMobileQuestion()$"
					}
             , sign :  {
                          eleg : {
                                     subhead : "$_pageDocumentForSignContentElegSubhead()$"
                                   , body1: " $_pageDocumentForSignContentElegBody1()$ "
                                   , clickHere : "$_pageDocumentForSignContentElegBody2ClickHere()$"
                                   , body2 : " $_pageDocumentForSignContentElegBody3()$ "
                                   , mobile : {
                                       startingMobileBankID : "Starta din BankID säkerhetsapp."
                                   }
                                 }
                    }
             , signByAuthor : {
                          modalTitle : "$_dialogConfirmSignByAuthorHead()$"
                        , eleg : {
                                     subhead : "$_dialogConfirmSignByAuthorElegSubHead()$"
                                   , body1: " $_dialogConfirmSignByAuthorElegBody()$ "
                                   , clickHere : "$_dialogConfirmSignByAuthorElegClickHere1()$"
                                   , body2 : " $_dialogConfirmSignByAuthorElegClickHere2()$"
                                 }

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
                       , sentViewer: "$_signatoryMessageSentViewer()$"
                       , openedViewer: "$_signatoryMessageOpenedViewer()$"
                       , waiting : "$_signatoryMessageWaiting()$"
                    }
            , authorAttachmentBoxHeader : "$_signViewNotsignedbymeAttachmentBoxHead()$"
            , evidenceAttachmentBoxHeader : "$_evidenceAttachmentBoxHeader()$"
            , signatoryAttachmentUploadButton: "$_signViewSigAttachmentBoxChooseFile()$"
            , step1template : "$_step1selectIfTemplate()$"
            , step1normal   : "$_step1selectIfElse()$"
            , step2template : "$_step2selectIfTemplate()$"
            , step2normal   : "$_step2selectIfNotTemplate()$"
            , saveAsTemplate : "$_signStepsMenuEmptyStepSaveAsTemplate()$"
            , saveAsDraft    : "$_saveAsDraft()$"
            , saveTemplate : "$_signStepsBodyFunctionssaveTemplate()$"
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
            , onlyFileWithTypeAllowed : function(t) {
                return "$_onlyFileWithTypeAllowed1()$" + t + "$_onlyFileWithTypeAllowed2()$"
              }
            , fileTooLarge: "$_fileTooLarge()$"
            , couldNotUpload: "$_couldNotUpload()$"
            , goBack: "$_goBack()$"
            , process : {
                contract: {
                   name: "$_contractname()$",
                   shortName: "$_contractshortname()$",
                   sendbuttontext: "$_contractsendbuttontext()$",
                   confirmsendtext: "$_contractconfirmsendtext()$",
                   confirmsendtitle: "$_contractconfirmsendtitle()$",
                   expirytext: "$_contractexpirytext()$",
                   restartbuttontext: "$_contractrestartbuttontext()$",
                   prolongbuttontext: "$_contractprolongbuttontext()$",
                   cancelbuttontext: "$_contractcancelbuttontext()$",
                   cancelmodaltitle: "$_contractcancelmodaltitle()$",
                   cancelmodaltext: "$_contractcancelmodaltext()$",
                   rejectbuttontext: "$_contractrejectbuttontext()$",
                   signatorysignmodaltitle: "$_contractsignatorysignmodaltitle()$",
                   signatorysignmodalcontent: "<p>$_contractsignatorysignmodalcontent()$ </p>",
                   signatorysignmodalcontentdesignvieweleg: "<p>$_contractsignatorysignmodalcontentdesignvieweleg()$ </p>",
                   signatorysignmodalcontentsignvieweleg: "<p>$_contractsignatorysignmodalcontentdesignvieweleg()$ </p>",
                   signatorysignmodalcontentauthoronly: "<p>$_contractsignatorysignmodalcontentauthoronly()$ </p>",
                   signbuttontext: "$_contractsignbuttontext()$",
                   signatorycancelmodaltitle: "$_contractsignatorycancelmodaltitle()$",
                   remindagainbuttontext: "$_contractremindagainbuttontext()$",
                   authorname: "$_contractauthorname()$",
                   authorsignatoryname: "$_contractauthorsignatoryname()$",
                   signatoryname: "$_contractsignatoryname()$",
                   nonsignatoryname: "$_contractnonsignatoryname()$"
                },
                offer: {
                   name: "$_offername()$",
                   shortName: "$_offershortname()$",
                   sendbuttontext: "$_offersendbuttontext()$",
                   confirmsendtext: "$_offerconfirmsendtext()$",
                   confirmsendtitle: "$_offerconfirmsendtitle()$",
                   expirytext: "$_offerexpirytext()$",
                   restartbuttontext: "$_offerrestartbuttontext()$",
                   prolongbuttontext: "$_offerprolongbuttontext()$",
                   cancelbuttontext: "$_offercancelbuttontext()$",
                   cancelmodaltitle: "$_offercancelmodaltitle()$",
                   cancelmodaltext: "$_offercancelmodaltext()$",
                   rejectbuttontext: "$_offerrejectbuttontext()$",
                   signatorysignmodaltitle: "$_offersignatorysignmodaltitle()$",
                   signatorysignmodalcontent: "<p>$_offersignatorysignmodalcontent()$ </p>",
                   signatorysignmodalcontentdesignvieweleg: "<p>$_offersignatorysignmodalcontentdesignvieweleg()$ </p>",
                   signatorysignmodalcontentsignvieweleg: "<p>$_offersignatorysignmodalcontentdesignvieweleg()$ </p>",
                   signatorysignmodalcontentauthoronly: "<p>$_offersignatorysignmodalcontentauthoronly()$ </p>",
                   signbuttontext: "$_offersignbuttontext()$",
                   signatorycancelmodaltitle: "$_offersignatorycancelmodaltitle()$",
                   remindagainbuttontext: "$_offerremindagainbuttontext()$",
                   authorname: "$_offerauthorname()$",
                   authorsignatoryname: "$_offerauthorsignatoryname()$",
                   signatoryname: "$_offersignatoryname()$",
                   nonsignatoryname: "$_offernonsignatoryname()$"
                },
                order: {
                   name: "$_ordername()$",
                   shortName: "$_ordershortname()$",
                   sendbuttontext: "$_ordersendbuttontext()$",
                   confirmsendtext: "$_orderconfirmsendtext()$",
                   confirmsendtitle: "$_orderconfirmsendtitle()$",
                   expirytext: "$_orderexpirytext()$",
                   restartbuttontext: "$_orderrestartbuttontext()$",
                   prolongbuttontext: "$_orderprolongbuttontext()$",
                   cancelbuttontext: "$_ordercancelbuttontext()$",
                   cancelmodaltitle: "$_ordercancelmodaltitle()$",
                   cancelmodaltext: "$_ordercancelmodaltext()$",
                   rejectbuttontext: "$_orderrejectbuttontext()$",
                   signatorysignmodaltitle: "$_ordersignatorysignmodaltitle()$",
                   signatorysignmodalcontent: "<p>$_ordersignatorysignmodalcontent()$ </p>",
                   signatorysignmodalcontentdesignvieweleg: "<p>$_ordersignatorysignmodalcontentdesignvieweleg()$ </p>",
                   signatorysignmodalcontentsignvieweleg: "<p>$_ordersignatorysignmodalcontentdesignvieweleg()$ </p>",
                   signatorysignmodalcontentauthoronly: "<p>$_ordersignatorysignmodalcontentauthoronly()$ </p>",
                   signbuttontext: "$_ordersignbuttontext()$",
                   signatorycancelmodaltitle: "$_ordersignatorycancelmodaltitle()$",
                   remindagainbuttontext: "$_orderremindagainbuttontext()$",
                   authorname: "$_orderauthorname()$",
                   authorsignatoryname: "$_orderauthorsignatoryname()$",
                   signatoryname: "$_ordersignatoryname()$",
                   nonsignatoryname: "$_ordernonsignatoryname()$"
                }

              }
            , filterByProcess: {
                showAllProcesses: "$_showAllProcesses()$",
                showAllProcessesTemplates: "$_showAllProcessesTemplates()$",
                showContractsOnly: "$_showContractsOnly()$",
                showOrdersOnly: "$_showOrdersOnly()$",
                showOffersOnly: "$_showOffersOnly()$"
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
           , eleg : "$_eleg()$"
           , editInviteText :"$_editinvitetextlinkInviteText()$"
           , editInviteDialogHead : "$_editInviteTextDialogHead()$"
           , days : "$_days()$"
           , to : "$_to()$"
           , nextStep : "$_signStepsBodyFunctionsNextStep()$"
           , uploadButton: "$_signStepsBodyUploadBoxStepButtonChooseFile()$"
           , ok : "$_ok()$"
           , addSignatory : "$_signStepsBodyFunctionsAddPerson()$"
           , removeSignatory : "$_signStepsBodyFunctionsRemovePerson()$"
           , isSigningParty : "$_authorsignatoryradioSenderSign()$"
           , isNotSigningParty : "$_authorsignatoryradioSenderNotSign()$"
           , signatoryListPartner : "$_signStep2ContentPersons()$"
           , signatoryListSignOrder : "$_signStep2ContentSignOrder()$"
           , authorattachments: {
                  changeAuthorAttachments : "$_addattachmentlink()$"
                , selectAttachments: "$_updateAttachmentsDialogHead()$"
                , selectAttachment : "$_updateAttachmentsDialogUpLoadChooseAttachment()$"
                , selectAttachmentsDescription: "$_updateAttachmentsDialogChooseAttachment()$"
                , storedInScrive: "$_updateAttachmentsDialogUpLoadSaved()$"
                , selectFile: "$_updateAttachmentsDialogUpLoadChooseFile()$"
                , selectedAttachments : "$_selectedAttachments()$"
                , invalidAttachments : "$_invalidAttachments()$"
            }
          , signatoryAttachments : {
                addAttachment:      "$_pageDocumentForAuthorHelpersLocalDialogsAddAttachment()$"
              , description:        "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentdesc()$"
              , attachment:         "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentname()$"
              , from :              "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentsigs()$"
              , requestAttachments : "$_pageDocumentForAuthorHelpersLocalDialogsHead()$"
            }
          , save : "$_save()$"
          , signLast : "$_switchercheckboxSignLast()$"
          , languages : {
                selectLanguage : "$_documentLanguageLabel()$"
              , en : "$_English()$"
              , sv : "$_Swedish()$"
              , changeEnglishToSwedishText :  "$_signInSwedishConfirmText()$"
              , signInSwedish : "$_signInSwedish()$"
              , changeSwedishToEnglishText: "$_signInEnglishConfirmText()$"
              , signInEnglish:  "$_signInEnglish()$"
            }
         , csv : {
                title : "$_dialogCsvupload1Head()$"
              , selectFileHeader : "$_dialogCsvupload1ChooseFileToUpLoad()$"
              , selectFileSubheader : "$_dialogCsvupload1CSVOnly()$"
              , selectFile :  "$_dialogCsvupload2CSVMultiChooseFile()$"
              , selectOtherFile: "$_dialogCsvupload2CSVMultiChooseFile()$"
            }
         , signature : {
               placeFor : function(s) { return s + " $_signaturePlaceFor()$"; }
             , placeYour : "$_signaturePlaceYour()$"
             , placeYourTip : "$_signaturePlaceYourTip()$"
             , confirmSignature : "$_signatureConfirm()$"
           }
         , designview : {
              selectprocess : "$_selectDocumentType()$",
              validation : {
                      missingOrWrongEmail : "$_emptyOrNotValidEmail()$"
                    , missingOrWrongPersonalNumber : "$_backToStepTwoAndFillPersonNumber()$"
                    , missingOrWrongMobile : "$_emptyOrNotValidMobile()$"
                    , missingOrWrongPlacedAuthorField : "$_missingPlacedAuthorFieldValue()$"
                    , sameMails : "$_repetedEmailAddress()$"
                    , atLeastOnePersonMustSigns : "$_atLeastOnePersonMustSigns()$"
                    , notReadyField : "$_setAllFields()$"
                    , notPlacedField : "$_placeAllFields()$"
                    , fileMustBeAdded : "$_fileMustBeAdded()$"
                    , pleaseSelectField : "$_validationPleaseSelectField()$"
                }
             , authentication : {
                      selectmethod: "$_signStepsBodyFunctionsBoxChooseAuthentication()$"
                }
             , delivery : {
                      selectmethod: "$_signStepsBodyFunctionsBoxChooseDelivery()$"
             }
             , messages : {
                      sendingDocument: "$_designViewMessageSending()$"
                    , reloadingDocument : "$_designViewMessageReloading()$"
                    , extraActions : "$_designViewMessageExtraActions()$"
                }
             , sign : "$_sign()$"
             , removeFile : "$_removeFile()$"
             , textFields : {
                      textField: "$_textField()$"
                    , obligatory: "$_textFieldObligatory()$"
                    , forThis : "$_textFieldFor()$"
                    , done : "$_textFieldDone()$"
                    , help : "$_textFieldHelp()$"
             }
             , checkboxes : {
                      checkbox : "$_checkboxCheckbox()$"
                    , prechecked : "$_checkboxPrechecked()$"
                    , obligatory: "$_checkboxObligatory()$"
                    , name : "$_checkboxName()$"
                    , forThis : "$_checkboxFor()$"
                    , done : "$_checkboxDone()$"
                    , help : "$_checkboxHelp()$"
             }
             , saveAsTemplateDescription : "$_saveAsTemplateLongDescription()$"
             , editParticipants: "$_designViewEditParticipants()$"
             , editDocument: "$_designViewEditDocument()$"
             , optional: "$_designViewOptional()$"
             , optionalField: "$_designViewOptionalField()$"
             , optionalFieldAbbrev: "$_designViewOptionalFieldAbbrev()$"
             , editSigningProcess: "$_designViewEditSigningProcess()$"
             , startSigning: "$_designViewStartSigning()$"
             , addField: "$_designViewAddField()$"
             , customField: "$_designViewCustomField()$"
             , fieldName: "$_designViewFieldName()$"
             , defineParticipation: "$_designViewDefineParticipation()$"
             , toReceiveDocument: "$_designViewToReceiveDocument()$"
             , byEmail: "$_designViewByEmail()$"
             , onThisTablet: "$_designViewOnThisTablet()$"
             , bySMS: "$_designViewBySMS()$"
             , byEmailAndSMS : "$_designViewbyEmailAndSMS()$"
             , forSigning: "$_designViewForSigning()$"
             , forViewing: "$_designViewForViewing()$"
             , noIDControl: "$_designViewNoIDControl()$"
             , withEleg: "$_designViewWithEleg()$"
             , addParty: "$_designViewAddParty()$"
             , addMultisend: "$_designViewAddMultisend()$"
             , viewCSV: "$_designViewViewCSV()$"
             , fullName: "$_designViewFullName()$"
             , optional: "$_designViewOptional()$"
             , mandatoryForRecipient: "$_designViewMandatoryForRecipient()$"
             , mandatoryForRecipientAbbrev: "$_designViewMandatoryForRecipientAbbrev()$"
             , mandatoryForSender: "$_designViewMandatoryForSender()$"
             , mandatoryForSenderAbbrev: "$_designViewMandatoryForSenderAbbrev()$"
             , documentName: "$_designViewDocumentName()$"
             , documentType: "$_designViewDocumentType()$"
             , recipientsLanguage: "$_designViewRecipientsLanguage()$"
             , signingDeadline: "$_designViewSigningDeadline()$"
             , addRemove: "$_designViewAddRemove()$"
             , request: "$_designViewRequest()$"
             , editInvitation: "$_designViewEditInvitation()$"
             , previewInvitation: "$_designViewPreviewInvitation()$"
             , dAndDActions: "$_designViewDAndDActions()$"
             , signatureBox: "$_designViewSignatureBox()$"
             , freeTextBox: "$_designViewFreeTextBox()$"
             , offsite: "$_designViewOffsite()$"
             , checkbox: "$_checkBoxFieldName()$"
             , removeThisDocument: "$_designViewRemoveThisDocument()$"
             , openPDF: "$_designViewOpenPDF()$"
             , chooseDocument: "$_designViewChooseDocument()$"
             , attached : "$_designViewAttached()$"
             , requestedFrom : "$_designViewRequestedFrom()$"
             , draggablehelp1 : "$_designviewdraggablehelp1()$"
             , draggablehelp2 : "$_designviewdraggablehelp2()$"
             , days :"$_designviewdays()$"
             , saved : "$_designviewsaved()$"
           }

         , validation : {
                firstNameRequired: "$_flashMessageFirstNameIsRequired()$"
              , secondNameRequired: "$_flashMessageSecondNameIsRequired()$"
              , required: "$_nameRequired()$"
              , toolong: "$_nameTooLong()$"
              , invalidnamechars: "$_nameInvalidChars()$"
              , firstNameField: "$_firstNameField()$"
              , lastNameField: "$_lastNameField()$"
              , fullNameField: "$_fullName()$"
              , companyNumberInvalidChars: "$_companyNumberInvalidChars()$"
              , companyNumberTooShort: "$_companyNumberTooShort()$"
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
                delivery : "$_padDelivery()$"
             ,  signingOnSameDevice : "$_padSigningOnSameDevice()$"
             ,  signingOnSameDeviceConfirmHeader : "$_padSigningOnSameDeviceConfirmHeader()$"
             ,  signingOnSameDeviceConfirmText : "$_padSigningOnSameDeviceConfirmText()$"
             ,  addToPadQueue : "$_padAddToPadQueue()$"
             ,  addToPadQueueConfirmHeader : "$_padAddToPadQueueConfirmHeader()$"
             ,  addToPadQueueConfirmText : "$_padAddToPadQueueConfirmText()$"
             ,  addToPadQueueNotAdded : "$_padAddToPadQueueNotAdded()$"
             ,  removeFromPadQueue : "$_padRemoveFromPadQueue()$"
             ,  drawSignatureBoxHeader : "$_padDrawSignatureBoxHeader()$"
             ,  signingOnSameDeviceFor :  "$_padSigningOnSameDeviceFor()$"
             ,  sendToDifferentDeviceFor :  "$_sendToDifferentDeviceFor()$"
             ,  justSend : "$_justSend()$"
             ,  howDoYouWantToSign : "$_howDoYouWantToSign()$"
             ,  cleanImage : "$_padCleanImage()$"
             ,  notNamedParty : "$_notNamedParty()$"
             ,  changePartyTo : "$_changePartyTo()$"
             ,  giveForSigningThisDevice : "$_padGiveForSigningThisDevice()$"
        }
         , mobile : {
                delivery : "$_mobileDelivery()$"
         }
         , changeEmail : "$_changeEmail()$"
         , changePhone : "$_changePhone()$"
         , onlyPDF : "$_onlyPDF()$"
         , newPasswordModal: {
             modalNewPasswordViewNewPassword: "$_modalNewPasswordViewNewPassword()$"
           , modalNewPasswordViewRepeatPassword: "$_modalNewPasswordViewRepeatPassword()$"
           , modalNewPasswordViewHeader: "$_modalNewPasswordViewHeader()$"
           , modalNewPasswordViewFooterSave: "$_modalNewPasswordViewFooterSave()$"
           , flashMessagePasswordsDontMatch: "$_flashMessagePasswordsDontMatch()$"
           , flashMessageMissingRequiredField: "$_flashMessageMissingRequiredField()$"
           , flashMessagePasswordLessThanMinLength: "$_flashMessagePasswordLessThanMinLength()$"
           , flashMessagePasswordExceedsMaxLength: "$_flashMessagePasswordExceedsMaxLength()$"
           , flashMessageInvalidCharsInPassword: "$_flashMessageInvalidCharsInPassword()$"
           , flashMessageNeedsLetterAndDigit: "$_flashMessageNeedsLetterAndDigit()$"
           , flashMessagePasswordChangeLinkNotValid: '$_flashMessagePasswordChangeLinkNotValid()$'
         }
         , signupModal: {
             modalAccountSetupFooter: "$_modalAccountSetupFooter()$"
           , mainHeader : "$_signupModalMainHeader()$"
           , alreadyHaveAnAccount : "$_alreadyHaveAccount()$"
         }
         , visitOurPricingPage : "$_visitOurPricingPage()$"
         , accountSetupModal: {
             modalAccountSetupBodyAccept: "$_modalAccountSetupBodyAccept()$"
           , modalAccountSetupBodyTOS   : "$_modalAccountSetupBodyTOS()$"
           , modalAccountSetupChoosePassword: "$_modalAccountSetupChoosePassword()$"
           , modalAccountSetupRepeatPassword: "$_modalAccountSetupRepeatPassword()$"
           , modalAccountSetupPhone: "$_modalAccountSetupPhone()$"
           , modalAccountSetupPosition : "$_modalAccountSetupPosition()$"
           , modalAccountSetupCompany : "$_modalAccountSetupCompany()$"
           , modalAccountOptionalPositions : "$_modalAccountOptionalPositions()$"
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
         }
         , history : {
               time  : "$_historyTime()$"
             , party : "$_historyParty()$"
             , description : "$_historyDescription()$"
             , expand : "$_historyExpand()$"
             , available : "$_historyAvailable()$"
             , hide : "$_historyHide()$"
           }
         , docsignview : {
               followArrowToSign: "$_followArrowToSign()$"
             , reviewDocument: "$_reviewDocument()$"
             , signedNotClosed: "$_signedNotClosed()$"
             , signedNotClosedSubText: "$_signedNotClosedSubText()$"
             , signedAndClosed: "$_signedAndClosed()$"
             , signedAndClosedSubText: "$_signedAndClosedSubText()$"
             , unavailableForSign: "$_unavailableForSign()$"
             , dueDate: "$_dueDate()$"
             , showDetails: "$_showDetails()$"
             , companyNumberLabel: "$_companyNumberLabel()$"
             , personalNumberLabel: "$_personalNumberLabel()$"
             , authorAttachmentsTitleForOne: "$_signViewAuthorAttachmentsTitleForOne()$"
             , authorAttachmentsTitleForLots: "$_signViewAuthorAttachmentsTitleForLots()$"
             , signatoryAttachmentsTitleForOne: "$_signViewSignatoryAttachmentsTitleForOne()$"
             , signatoryAttachmentsTitleForLots: "$_signViewSignatoryAttachmentsTitleForLots()$"
             , signatoriesTitle: "$_signViewSignatoriesTitle()$"
             , newAccountTitle: "$_signViewCreateAccountTitle()$"
             , newAccountSubTitle: "$_signViewCreateAccountSubTitle()$"
             , emailLabel: "$_email()$"
             , passwordLabel: "$_modalAccountSetupChoosePassword()$"
             , password2Label: "$_modalAccountSetupRepeatPassword()$"
             , acceptTOSLabel: "$_accountFromSignInputsBodyAcceptTOS()$"
             , newAccountButton: "$_signViewCreateAccountLabel()$"
             , createdAccountTitle: "$_signViewCreatedAccountTitle()$"
             , createdAccountSubtitle: "$_signViewCreatedAccountSubtitle()$"
             , shareTitle: "$_signViewShareTitle()$"
             , facebookButtonLabel: "$_facebookButtonLabel()$"
             , tweetButtonLabel: "$_tweetButtonLabel()$"
             , phoneButtonLabel: "$_phoneButtonLabel()$"
             , phoneFormDescription: "$_phoneFormDescription()$"
             , phoneSubmitButtonLabel: "$_phoneSubmitButtonLabel()$"
             , phoneConfirmationText: "$_phoneConfirmationText()$"
             , startButtonLabel: "$_startButtonLabel()$"
             , notEntered: "$_notEntered()$"
             , checkboxes : {
                    pleaseCheck : "$_checkboxPleaseCheck()$"
               }
             , filladitionfields : "$_filladitionfields()$"
             , filladitionfieldsdescription : "$_filladitionfieldsdescription()$"
             , welcome : "$_welcome()$"

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
        , avtal24 : {
             buy :   "$_buyAvtal24Template()$"
           , go :    "$_goToAvtal24()$"
           , title : "$_avtal24Title()$"
           , description : "$_avtal24Description()$"
          }
        , promo : {
             accept : "$_promoAccept()$"
           , decline : "$_promoDecline()$"
        }
        , payments: {
             users: "$_CompanyAccounts()$"
           , user: "$_paymentsUser()$"
           , vat: "$_paymentsVat()$"
           , vat25: "$_paymentsVat25()$"
           , expires: "$_paymentsExpires()$"
           , creditcardnumber: "$_paymentsCreditCardNumber()$"
           , subheader: "$_paymentsSubheader()$"
           , contact: "$_paymentPlanContact()$"
           , already: "$_paymentPlanAlreadySubscribed()$"
           , alreadyGo: "$_paymentPlanAlreadySubscribedGo()$"
           , placeholder : "$_paymentPlanPlaceholder()$"
           , sendmsg : "$_paymentPlanSendMessage()$"
           , plans: {
                      team         : {
                      name : "$_paymentPlanTeam()$",
                      tag :"$_paymentPlanTeamTag()$",
                      price : "$_paymentsTeamPrice()$<span class='aster'>*</span>",
                      price3 : "$_paymentPlanPerUserPerMonth()$"
                    }
                    , form : {
                      name : "$_paymentPlanForm()$",
                      tag : "$_paymentPlanFormTag()$",
                      price : "$_paymentPlanFormPrice()$<span class='aster'>*</span>",
                      price3 : "$_paymentPlanPerMonthPerDoc()$"
                    }
                    , enterprise : {
                      name : "$_paymentPlanEnterprise()$",
                      tag : "$_paymentPlanEnterpriseTag()$",
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
            , chooseplan : "$_paymentsChoosePlan()$"
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
                            , type   : "$_SortType()$"

                       }
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
                            , type       : "$_SortType()$"
                            , template   : "$_SortTemplate()$"
                            , shared      : "$_share()$"
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
                            , shared      : "$_share()$"
                            , attachment   : "$_SortAttachment()$"

                       }
                }
            ,  bin: {
                       name :  "$_docListTabsBin()$"
                    ,  remove : {
                              action: "$_delete()$"
                            , head : "$_listReallyDeleteDialogHead()$"
                            , body : "$_listReallyDeleteDialogBody()$"
                            , successMessage : "$_flashMessageRubbishHardDeleteDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  restore : {
                              action: "$_restore()$"
                            , head : "$_listRestoreDialogHead()$"
                            , body : "$_listRestoreDialogBody()$"
                            , successMessage : "$_flashMessageRubbishRestoreDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
                        }
                    ,  search : "$_SearchDoc()$"
               }
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
              headline : "$_blockingFreeHasHeadline()$",
              subtext1 : "$_blockingFreeHasSubtext1()$"
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
            deleteToken :  "$_apiDashboardDelete()$"
          , deleteAll : "$_apiDashboardDeleteAll()$"
          , grantedPriviligesDescription: "$_apiDashboardGrantedPriviligesDescription()$"
          , personalTokenDescription: "$_apiDashboardpersonalTokenDescription()$"
          , personalTokenCreate: "$_apiDashboardPersonalTokenCreate()$"
          , apiTokenDescription : "$_apiDashboardApiTokenDescription()$"
          , apiTokenCreate : "$_apiDashboardApiTokenCreate()$"
          , personOrCompany : "$_apiDashboardPersonOrCompany()$"
          , privilige       :  "$_apiDashboardPrivilige()$"
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
              , createCompany : "$_createCompany()$"
              , createCompanyExplanation : "$_createCompanyExplanation()$"
              , createCompanyPreambule :   "$_createCompanyFormPreamble()$"
              , save : "$_SaveChanges()$"
              , detailSaved : "$_flashMessageUserDetailsSaved()$"
              , mismatchedEmails : "$_flashMessageMismatchedEmails()$"
              , invalidEmail : "$_flashMessageInvalidEmailFormat()$"
              , emailAlreadyInUse : "$_flashCantChangeEmail()$"
              , changeEmailMailSent1 : "$_flashMessageChangeEmailMailSent1()$"
              , changeEmailMailSent2 : "$_flashMessageChangeEmailMailSent2()$"
              , companyCreated : "$_flashMessageCompanyCreated()$"

            }
          , accountSecurity : {
                name            : "$_HeadAccountSettings()$"
              , passwordSection : "$_password()$"
              , oldpassword     : "$_PresentPassword()$"
              , newpassword1    : "$_NewPassword()$"
              , newpassword2    : "$_RepeatNewPassword()$"
              , langSection     : "$_LanguageSettings()$"
              , lang            : "$_UserLanguage()$"
              , langSV          : "$_Swedish()$"
              , langEN          : "$_English()$"
              , footerSection   : "$_userDocumentsSettings()$"
              , useFooter       : "$_userDocumentSettingsMailFooter()$"
              , ipMaskSection   : "$_userIPMaskSection()$"
              , save : "$_SaveChanges()$"
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
              , columnName :  "$_name()$"
              , columnEmail : "$_email()$"
              , columnRole :  "$_role()$"

            }
          , mailAPI : {
                name : "$_HeadMailApi()$"
              , save : "$_SaveChanges()$"
              , activeMails : "$_MailAPIActiveEmail()$"
              , dailyLimit : "$_MailAPILimitsDaily()$"
              , processedToday : "$_MailAPILimitsProcessedToday()$"
              , personalMailAPIHeader : "$_MailAPISettings()$"
              , enablePersonal : "$_MailAPIEnable()$"
              , resetKeys : "$_MailAPIResetKey()$"
              , limitMailAPIHeader : "$_MailAPILimitHead()$"
              , resetLimit : "$_MailAPILimitsResetToday()$"
              , companyMailAPIHeader : "$_CompanyMailAPI()$"

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
      , companyBranding : {
               customiseHeaderFontLabel : "$_customiseHeaderFontLabel()$"
             , customiseFontLabel : "$_customiseFontLabel()$"
             , customiseSignViewFooterTextColourLabel : "$_customiseSignViewFooterTextColourLabel()$"
             , customiseSignViewFooterTextFontLabel : "$_customiseSignViewFooterTextFontLabel()$"
             , customiseSignViewHeaderTextColourLabel : "$_customiseSignViewHeaderTextColourLabel()$"
             , customiseSignViewHeaderTextFontLabel : "$_customiseSignViewHeaderTextFontLabel()$"
             , customiseSignViewHeaderBackgroundColour : "$_customiseSignViewHeaderBackgroundColour()$"
             , customiseSignViewFooterBackgroundColour : "$_customiseSignViewFooterBackgroundColour()$"
             , customiseSignViewBackgroundColour : "$_customiseSignViewBackgroundColour()$"
             , signviewBranding : "$_signviewBranding()$"
             , serviceBranding : "$_serviceBranding()$"
             , customiseBorderColourLabel : "$_customiseBorderColourLabel()$"
             , customiseButtonColourLabel : "$_customiseButtonColourLabel()$"
             , customiseEmailBackgroundColour : "$_customiseEmailBackgroundColourLabel()$"
             , sampleEmailButtonLabel : "$_sampleEmailButtonLabel()$"
             , sampleEmailFooter : "$_sampleEmailFooter()$"
             , barsColour : "$_companyBrandingBarsColour()$"
             , barsTextColour : "$_companyBrandingBarsTextColour()$"
             , barsSecondaryColour : "$_companyBrandingbarsSecondaryColour()$"
             , saveBranding: "$_SaveSettings()$"
             , emailBranding: "$_EmailBranding()$"
             , customiseLogo: "$_CustomiseLogo()$"
             , customiseBackgroundColour: "$_CustomiseBackgroundColour()$"
             , customiseTextColour: "$_CustomiseTextColour()$"
             , selectImageLabel: "$_SelectImageLabel()$"
             , sampleEmailHeader: "$_SampleEmailHeader()$"
             , sampleEmailSubheader: "$_mailsubheader()$"
             , sampleEmailContent: "$_SampleEmailContent()$"
             , customize : "$_companyBrandingCustomize()$"
      }
      , dates : {
               jan : "$_datesJan()$"
             , feb : "$_datesFeb()$"
             , mar : "$_datesMar()$"
             , apr : "$_datesApr()$"
             , may : "$_datesMay()$"
             , jun : "$_datesJun()$"
             , jul : "$_datesJul()$"
             , aug : "$_datesAug()$"
             , sep : "$_datesSep()$"
             , oct : "$_datesOct()$"
             , nov : "$_datesNov()$"
             , dec : "$_datesDec()$"
      }
      , createFromTemplateDescription : "$_createFromTemplateDescription()$"
      , nofiletext : "$_nofiletext()$"
    };
###
