####
#### This module puts in global context object 'localization'
#### If some script requires some string to be shown to the user
#### it just uses this module
####
javascriptLocalisation =
    var localization =
        {
               loadingFile : "$_loadingFile()$"
             , problemWithPDF : "$_problemWithPDF()$"
             , backToArchive : "$_backToArchive()$"
             , repetedEmailAddress : "$_repetedEmailAddress()$"
             , offerSignatory : "$_offerSignatory()$"
             , sigattachmessage : "$_pageDocumentForAuthorHelpersLocalDialogsRecipient()$"
             , secretaryCantSign : "$_secretaryCantSign()$"
             , more : "$_more()$"
             , cantSendReminder: function(listtype){
                    return "$_cantSendReminder1()$" + listtype + "$_cantSendReminder2()$"
                    }
             , addRequiredAttachments : "$_addRequiredAttachments()$"
             , setAllFields : "$_setAllFields()$"
             , multipleSignatory : "$_multipleSignatory()$"
             , contractSignatory : function(idx) {return "$_contractSignatory()$ " + idx;}
             , nonsignatory : "$_nonsignatory()$"
             , offerAuthor : "$_offerAuthor()$"
             , author : "$_sender()$"
             , youMustSetParnerEmail : "$_youMustSetParnerEmail()$"
             , emptyOrNotValidEmail :  "$_emptyOrNotValidEmail()$"
             , atLeastOneSignatoryRequired : "$_atLeastOneSignatoryRequired()$"
             , mustFillFieldsBeforeSigning : "$_mustFillFieldsBeforeSigning()$"
             , backToStepTwoAndFillPersonNumber : "$_backToStepTwoAndFillPersonNumber()$"
             , missingNames : "$_missingNames()$"
             , missingSignatoryNames : "$_missingSignatoryNames()$"
             , yourSigningPluginFailed : "$_yourSigningPluginFailed()$"
             , startingSaveSigning : "$_startingSaveSigning()$"
             , noNordeaInstalled  : "$_noNordeaInstalled()$"
             , noTeliaInstalled  : "$_noTeliaInstalled()$"
             , noBankIdInstalled  : "$_noBankIdInstalled()$"
             , verifyingSignature : "$_verifyingSignature()$"
             , tbsGenerationMessage : function(doctitle, docid){
                    return  '$_tbsGenerationMessage1()$"' + doctitle + '"$_tbsGenerationMessage2()$"' + docid + '"$_tbsGenerationMessage3()$'
                     }
             , differentSignatories :  "$_differentSignatories()$"
             , ie6NotSupported : "$_ie6NotSupported()$"
             , youCanNotInviteSameEmail : "$_youCanNotInviteSameEmail()$"
             , fstname : "$_firstname()$"
             , sndname : "$_lastname()$"
             , companyposition : "$_companyposition()$"
             , phone : "$_phone()$"
             , email : "$_email()$"
             , password : "$_password()$"
             , niceEmail : "$_niceEmail()$"
             , company : "$_company()$"
             , personamNumber : "$_personnumber()$"
             , companyNumber : "$_orgnumber()$"
             , loadingTweets : "$_loadingTweets()$"
             , fieldName : "$_fieldName()$"
             , multiPartPartner : "$_multiPartPartner()$"
             , noNamePerson : "$_noNamePerson()$"
             , senderName : "$_senderName()$"
             , senderTitle : "$_senderTitle()$"
             , senderNumber :  "$_senderNumber()$"
             , senderEmail :  "$_senderEmail()$"
             , partnerName : "$_partnerName()$"
             , partnerTitle : "$_partnerTitle()$"
             , partnerNumber : "$_partnerNumber()$"
             , partnerEmail : "$_partnerEmail()$"
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
             , select : "$_options()$"
             , of : "$_of()$"
             , and : "$_and()$"
             , listand : "$_listand()$"
             , you : "$_you()$"
             , searchBoxButtonText : "$_searchBoxButtonText()$"
             , downloadPDF : "$_DownloadPDF()$"
             , reviewPDF: "$_ReviewPDF()$"
             , deletePDF: "$_DeletePDF()$"
             , document : "$_Document()$"
             , onlyPDFAllowed : "$_updateAttachmentsDialogUpLoadOnlyPDF()$"
             , attachmentType : "$_pageAttachmentAttachments()$" 
             , attachment : "$_Attachment()$"
             , attachmentsWord : "$_Attachments()$"
             , requestedAttachments : "$_RequestedAttachments()$"
             , send : "$_send()$"
             , csvFilePersons : "$_csvFilePersons()$"
			 , reminder : {
					     send : "$_sendReminder()$"
					   , formHead : "$_reminderFormHead()$"
					   , formSend :	"$_reminderFormSendReminder()$"
                       , formOwnMessage : "$_reminderFormFooterOwnMessage()$"
					}
             , sign :  {
                          guardCBox : "$_signGuardCBox()$"
                        , eleg : {
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
                        , modalBody1 : "$_dialogConfirmSignByAuthorBody1()$"
                        , modalBody2 : "$_dialogConfirmSignByAuthorBody2()$"
                        , modalBody3 : "$_dialogConfirmSignByAuthorBody3()$"
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
                    }
            , authorAttachmentBoxHeader : "$_signViewNotsignedbymeAttachmentBoxHead()$"
            , signatoryAttachmentBoxHeader : "$_signViewSigAttachmentBoxHead()$"
            , signatoryAttachmentUploadButton: "$_signViewSigAttachmentBoxChooseFile()$"
            , uploadedAttachments: "$_uploadedAttachments()$"
            , waitingForAttachment: "$_waitingForAttachment()$"
            , templateTitle : "$_signStepsTitleRowTextContainerIfTemplate()$"
            , step1template : "$_step1selectIfTemplate()$"
            , step1normal   : "$_step1selectIfElse()$"
            , step2template : "$_step2selectIfTemplate()$"
            , step2normal   : "$_step2selectIfNotTemplate()$"
            , saveAsTemplate : "$_signStepsMenuEmptyStepSaveAsTemplate()$"
            , saveTemplate : "$_signStepsBodyFunctionssaveTemplate()$"
            , skrivapa : "$_skrivapa()$"

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

            , tweetAccount : "$_tweetAccount()$"
            , selectFileToUpload : "$_selectFileToUpload()$"
            , onlyFileWithTypeAllowed : function(t) {
                return "$_onlyFileWithTypeAllowed1()$" + t + "$_onlyFileWithTypeAllowed2()$"
              }
            , fileTooLarge: "$_fileTooLarge()$"
            , couldNotUpload: "$_couldNotUpload()$"
            , process : {
                contract: {
                   name: "$_contractname()$",
                   shortName: "$_contractshortname()$",
                   title: "$_contracttitle()$",
                   sendbuttontext: "$_contractsendbuttontext()$",
                   confirmsendtext: "$_contractconfirmsendtext()$",
                   confirmsendtitle: "$_contractconfirmsendtitle()$",
                   expirytext: "$_contractexpirytext()$",
                   restartbuttontext: "$_contractrestartbuttontext()$",
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
                   signbuttontext: "$_contractsignbuttontext()$",
                   signbuttontextauthor: "$_contractsignbuttontextauthor()$",
                   signatorycancelmodaltitle: "$_contractsignatorycancelmodaltitle()$",
                   authorissecretarytext: "$_contractauthorissecretarytext()$",
                   remindagainbuttontext: "$_contractremindagainbuttontext()$",
                   authorname: "$_contractauthorname()$",
                   authorsignatoryname: "$_contractauthorsignatoryname()$",
                   signatoryname: "$_contractsignatoryname()$",
                   nonsignatoryname: "$_contractnonsignatoryname()$"
                },
                offer: {
                   name: "$_offername()$",
                   shortName: "$_offershortname()$",
                   title: "$_offertitle()$",
                   sendbuttontext: "$_offersendbuttontext()$",
                   confirmsendtext: "$_offerconfirmsendtext()$",
                   confirmsendtitle: "$_offerconfirmsendtitle()$",
                   expirytext: "$_offerexpirytext()$",
                   restartbuttontext: "$_offerrestartbuttontext()$",
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
                   signbuttontext: "$_offersignbuttontext()$",
                   signbuttontextauthor: "$_offersignbuttontextauthor()$",
                   signatorycancelmodaltitle: "$_offersignatorycancelmodaltitle()$",
                   authorissecretarytext: "$_offerauthorissecretarytext()$",
                   remindagainbuttontext: "$_offerremindagainbuttontext()$",
                   authorname: "$_offerauthorname()$",
                   authorsignatoryname: "$_offerauthorsignatoryname()$",
                   signatoryname: "$_offersignatoryname()$",
                   nonsignatoryname: "$_offernonsignatoryname()$"
                },
                order: {
                   name: "$_ordername()$",
                   shortName: "$_ordershortname()$",
                   title: "$_ordertitle()$",
                   sendbuttontext: "$_ordersendbuttontext()$",
                   confirmsendtext: "$_orderconfirmsendtext()$",
                   confirmsendtitle: "$_orderconfirmsendtitle()$",
                   expirytext: "$_orderexpirytext()$",
                   restartbuttontext: "$_orderrestartbuttontext()$",
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
                   signbuttontext: "$_ordersignbuttontext()$",
                   signbuttontextauthor: "$_ordersignbuttontextauthor()$",
                   signatorycancelmodaltitle: "$_ordersignatorycancelmodaltitle()$",
                   authorissecretarytext: "$_orderauthorissecretarytext()$",
                   remindagainbuttontext: "$_orderremindagainbuttontext()$",
                   authorname: "$_orderauthorname()$",
                   authorsignatoryname: "$_orderauthorsignatoryname()$",
                   signatoryname: "$_ordersignatoryname()$",
                   nonsignatoryname: "$_ordernonsignatoryname()$"
                }

              }
            , filterByProcess: {
                showAllProcesses: "$_showAllProcesses()$",
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
           , searchTemplate : "$_SearchTemplate()$"
           , sortTemplate : "$_SortTemplate()$"
           , infoSelectTemplate : "$_InfoSelectTemplate()$"
           , goBack : "$_GoBack()$"
           , eleg : "$_eleg()$"
           , finalDateCheckboxLabel : "$_daystosigncheckboxExpDate()$"
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
                , attach: "$_updateAttachmentsDialogUpLoadFooterAttach()$"
                , selectAttachments: "$_updateAttachmentsDialogHead()$"
                , selectAttachment : "$_updateAttachmentsDialogUpLoadChooseAttachment()$"
                , selectAttachmentsDescription: "$_updateAttachmentsDialogChooseAttachment()$"
                , storedInScrive: "$_updateAttachmentsDialogUpLoadSaved()$"
                , selectFile: "$_updateAttachmentsDialogUpLoadChooseFile()$"
                , selectedAttachments : "Selected attachments"
            }
          , signatoryAttachments : {
                addAttachment:      "$_pageDocumentForAuthorHelpersLocalDialogsAddAttachment()$"
              , description:        "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentdesc()$"
              , attachment:         "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentname()$"
              , from :              "$_pageDocumentForAuthorHelpersLocalDialogsAttachmentsigs()$"
              , requestAttachments : "$_pageDocumentForAuthorHelpersLocalDialogsHead()$"
              , select            : "$_pageDocumentForAuthorHelpersLocalDialogsRecipient()$"
            }
          , save : "$_save()$"
          , signLast : "$_switchercheckboxSignLast()$"
          , languages : {
                selectLanguage : "$_documentLanguageLabel()$"
              , en : "$_English()$"
              , se : "$_Swedish()$"
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
             , missing : "$_signatureMissing()$"
             , placeYour : "$_signaturePlaceYour()$"
             , placeAndSign : "$_signatureSign()$"
             , confirmSignature : "$_signatureConfirm()$"
           }
         , designview : {
              selectprocess : "$_selectDocumentType()$",
              validation : {
                      missingOrWrongEmail : "$_emptyOrNotValidEmail()$"
                    , missingNames:  "$_missingSignatoryNames()$"
                    , wrongNames :   "$_wrongSignatoryNames()$"
                    , missingOrWrongPersonalNumber : "$_backToStepTwoAndFillPersonNumber()$"
                    , missingOrWrongPlacedAuthorField : "$_missingPlacedAuthorFieldValue()$"
                    , missingOrWrongCustomFieldValue: "$_missingCustomFieldValue()$"
                    , sameMails : "$_repetedEmailAddress()$"
                    , atLeastOnePersonMustSigns : "$_atLeastOnePersonMustSigns()$"
                    , notReadyField : "$_setAllFields()$"
                    , notPlacedField : "$_placeAllFields()$"
                    , fileMustBeAdded : "$_fileMustBeAdded()$"
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
             , removeFile : "Remove file"
             , checkboxes : {
                      checkbox : "$_checkboxCheckbox()$"
                    , prechecked : "$_checkboxPrechecked()$"
                    , obligatory: "$_checkboxObligatory()$"
                    , name : "$_checkboxName()$"
                    , forThis : "$_checkboxFor()$"
                    , done : "$_checkboxDone()$"
                    , help : "$_checkboxHelp()$"
             } 
           }

         , validation : {
                firstNameRequired: "$_flashMessageFirstNameIsRequired()$"
              , secondNameRequired: "$_flashMessageSecondNameIsRequired()$"
              , passwordExceedsMaxLength: "$_PasswordValidationMessageInputExceedsMaxLength()$"
              , passwordLessThanMinLength: "$_PasswordValidationMessageInputLessThanMinLength()$"
              , passwordNeedsLetterAndDigit: "$_PasswordValidationMessageNeedsLetterAndDigit()$"
              , passwordsDontMatch: "$_PasswordValidationMessagePasswordsDontMatch()$"
              , mustAcceptTOS: "$_flashMessageMustAcceptTOS()$"
              , phoneRequired: "$_flashMessagePhoneIsRequired()$"
         }
         , emailBranding: "$_EmailBranding()$"
         , customiseBackgroundColour: "$_CustomiseBackgroundColour()$"
         , customiseTextColour: "$_CustomiseTextColour()$"
         , customiseLogo: "$_CustomiseLogo()$"
         , selectImageLabel: "$_SelectImageLabel()$"
         , saveBranding: "$_SaveSettings()$"
         , pad : {
                delivery : "$_padDelivery()$"
             ,  signingOnSameDevice : "$_padSigningOnSameDevice()$"
             ,  signingOnSameDeviceConfirmHeader : "$_padSigningOnSameDeviceConfirmHeader()$"
             ,  signingOnSameDeviceConfirmText : "$_padSigningOnSameDeviceConfirmText()$"
             ,  addToPadQueue : "$_padAddToPadQueue()$"
             ,  addToPadQueueConfirmHeader : "$_padAddToPadQueueConfirmHeader()$"
             ,  addToPadQueueConfirmText : "$_padAddToPadQueueConfirmText()$"
             ,  addToPadQueueAdded : "$_padAddToPadQueueAdded()$"
             ,  addToPadQueueNotAdded : "$_padAddToPadQueueNotAdded()$"
             ,  removeFromPadQueue : "$_padRemoveFromPadQueue()$"
             ,  drawSignatureBoxHeader : "$_padDrawSignatureBoxHeader()$"
             ,  signingOnSameDeviceFor :  "$_padSigningOnSameDeviceFor()$"
             ,  sendToDifferentDeviceFor :  "$_sendToDifferentDeviceFor()$"
             ,  justSend : "$_justSend()$"
             ,  howDoYouWantToSign : "$_howDoYouWantToSign()$"
             ,  cleanImage : "$_padCleanImage()$"
             ,  notNamedParty : "$_notNamedPartyPad()$"
             ,  documentCreatedFromTemplate : "$_documentCreatedFromTemplate()$"
             ,  maybeYouWantToSendDirectly : "$_maybeYouWantToSendDirectly()$"
             ,  sendDirectly : "$_sendDirectly()$"
             ,  goToDesignView : "$_goToDesignView()$"
        }
         , sampleEmailHeader: "$_SampleEmailHeader()$"
         , sampleEmailSubheader: "$_mailsubheader()$"
         , sampleEmailContent: "$_SampleEmailContent()$"
         , changeEmail : "$_changeEmail()$"
         , onlyPDF : "$_onlyPDF()$"
         , login : {
              logMeIn : "$_login()$"
         }
         , cancel : "$_cancel()$"
         , docsignview : {
               followArrowToSign: "$_followArrowToSign()$"
             , followArrowToReview: "$_followArrowToReview()$"
             , signedNotClosed: "$_signedNotClosed()$"
             , signedNotClosedSubText: "$_signedNotClosedSubText()$"
             , signedAndClosed: "$_signedAndClosed()$"
             , signedAndClosedSubText: "$_signedAndClosedSubText()$"
             , unavailableForSign: "$_unavailableForSign()$"
             , dueDate: "$_dueDate()$"
             , showDetails: "$_showDetails()$"
             , hideDetails: "$_hideDetails()$"
             , companyNumberLabel: "$_companyNumberLabel()$"
             , personalNumberLabel: "$_personalNumberLabel()$"
             , authorAttachmentsTitleForOne: "$_signViewAuthorAttachmentsTitleForOne()$"
             , authorAttachmentsTitleForLots: "$_signViewAuthorAttachmentsTitleForLots()$"
             , signatoryAttachmentsTitleForOne: "$_signViewSignatoryAttachmentsTitleForOne()$"
             , signatoryAttachmentsTitleForLots: "$_signViewSignatoryAttachmentsTitleForLots()$"
             , uploadedAttachmentsTitleForOne: "$_signViewUploadedAttachmentsTitleForOne()$"
             , uploadedAttachmentsTitleForLots: "$_signViewUploadedAttachmentsTitleForLots()$"
             , signatoriesTitle: "$_signViewSignatoriesTitle()$"
             , newAccountTitle: "$_signViewCreateAccountTitle()$"
             , newAccountSubTitle: "$_signViewCreateAccountSubTitle()$"
             , emailLabel: "$_email()$"
             , passwordLabel: "$_modalAccountSetupChoosePassword()$"
             , password2Label: "$_modalAccountSetupRepeatPassword()$"
             , acceptTOSLabel: '$_accountFromSignInputsBodyAcceptTOS()$'
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
             , contact: "$_contact()$"
             , checkboxes : {
                    pleaseCheck : "$_checkboxPleaseCheck()$" 
               }
             , filladitionfields : "$_filladitionfields()$"
             , filladitionfieldsdescription : "$_filladitionfieldsdescription()$"

        }
        , poweredByScrive : "$_poweredByScrive()$"
        , verification : {
                result: "Verification result"
             ,  success: "Valid signature"
             ,  failed: "Not digital signature"
             ,  failedMessage: "Given document was not signed using Scrive."
             ,  error : "Error"
             ,  errorMessage : "Error occured while trying to verify signature. Please try again."
             ,  time: "Time"
             ,  gateway : "Gateway"
             ,  reason: "Reason"

          }
       , uploadView : {
                back : "$_uploadViewGoBack()$"
             ,  startNewProcess : "$_signStepsBodyUploadBoxStep1StartNewProcess()$"
             ,  chooseTemplate : "$_signStepsBodyUploadBoxStep1ChooseTemplate()$"
             ,  newProcessFromFile: "$_signStepsBodyUploadBoxStep1NewProcessFromFile()$"
             ,  uploadNewTemplate : "$_signStepsBodyUploadBoxStep1UploadNewTemplate()$"
             ,  createNewTemplate : "$_createNewTemplate()$"
             ,  buyAvtal24Template : "$_buyAvtal24Template()$"
        }
        , payments: {
             users: "$_CompanyAccounts()$"
           , user: "$_paymentsUser()$"
           , expires: "$_paymentsExpires()$"
           , creditcardnumber: "$_paymentsCreditCardNumber()$"
           , manyusers: "$_paymentsManyUsers()$"
           , price : "$_paymentsPrice()$"
           , buysyou : "$_paymentsBuysYou()$"
           , phonenumber : "$_paymentsPhonenumber()$"
           , readmore : "$_paymentsReadMore()$"
           , featuresurl : "$_paymentsFeaturesURL()$"
           , plans: { professional : "$_paymentPlanProfessional()$"
                    , team         : {
                      name : "$_paymentPlanTeam()$",
                      tag :"$_paymentPlanTeamTag()$",
                      price : "$_paymentsTeamPrice()$",
                      features: [ "$_paymentFeatureDistance()$"
                                 , "$_paymentFeaturePad()$"
                                 , "$_paymentFeatureBranding()$"
                                 , "$_paymentFeatureTracking()$"
                                 , "$_paymentFeatureAPI()$"
                                 , "$_paymentFeatureFeatures()$"
                                 ],
                      price1 : "",
                      price3 : "$_paymentPlanPerUserPerMonth()$",
                      price21 : "",
                      price31 : "",
                      select : "$_paymentsSelect()$",
                      selected : "$_paymentsSelected()$"
                    }
                    , form : {
                      name : "$_paymentPlanForm()$",
                      tag : "$_paymentPlanFormTag()$",
                      price : "$_paymentPlanFormPrice()$",
                      features : [ "$_paymentFeatureSupport()$"
                                 , "$_paymentFeatureHTTPAPI()$"
                                 , "$_paymentFeatureDocumentation()$"
                                 , "$_paymentFeatureExampleCode()$"
                                 ],
                      price1 : "",
                      price3 : "$_paymentPlanPerMonth()$ $_and()$",
                      price21 : "$_paymentPlanDocPrice()$",
                      price31 : "$_paymentPlanPerDocument()$",
                      select : "$_paymentsGetInTouch()$",
                      selected : "$_paymentsSelected()$"
                    }
                    , enterprise : {
                      name : "$_paymentPlanEnterprise()$",
                      tag : "$_paymentPlanEnterpriseTag()$",
                      price : "$_paymentsQuote()$",
                      features : [ "$_paymentFeatureAllTeamFeatures()$"
                                 , "$_paymentFeatureAllOnlineFormFeatures()$"
                                 , "&nbsp;"
                                 , "$_paymentFeatureOnPremise()$"
                                 , "$_paymentFeatureFullCust()$"
                                 , "$_paymentFeatureProjMan()$"
                                 ],
                      price1 : "$_paymentsAskForA()$",
                      price3 : "",
                      price21 : "",
                      price31 : "",
                      select : "$_paymentsGetInTouch()$",
                      selected : "$_paymentsSelect()$"

                    }
                    , free         : "$_paymentPlanFree()$"
                    , pay          : "$_paymentPlanPay()$"
                    , logo : "$_paymentPlanLogoAndBranding()$"
                    , dash : "$_paymentPlanDashboard()$"
                    , docprice : "$_paymentPlanDocPrice()$"
                    }
           , table: { currentplan : "$_paymentPlanCurrentPlan()$"
                    , item        : "$_paymentPlanItem()$"
                    , price       : "$_paymentPlanPrice()$"
                    , quantity    : "$_paymentPlanQuantity()$"
                    , subtotal    : "$_paymentPlanSubtotal()$"
                    , total       : "$_paymentPlanTotal()$"
                    , payments    : "$_paymentPlanPayments()$"
                    , nextpayment : "$_paymentPlanNextPayment()$"
                    , previouspayments : "$_paymentPlanPreviousPayments()$"
                    , changesubscription : "$_paymentPlanChangeSubscription()$"
                    , changebilling: "$_paymentPlanChangeBilling()$"
                    , none: "$_paymentPlanNone()$"
                    }
            , features: { send     : "$_paymentFeatureSend()$"
                        , manage   : "$_paymentFeatureManage()$"
                        , dash     : "$_paymentFeatureDash()$"
                        , branding : "$_paymentFeatureBranding()$"
                        , features : "$_paymentFeatureFeatures()$"
                        }           
            , subscribe: "$_paymentPlanSubscribe()$"
            , pleaseleavenumbertitle: "$_paymentsPleaseLeaveNumberTitle()$"
            , ok: "$_paymentsSubmitButtonLabel()$"
            , pleaseleavenumber: "$_paymentsPleaseLeaveNumber()$"
            , askfora: "$_paymentsAskForA()$"
            , quote: "$_paymentsQuote()$"
            , getintouch: "$_paymentsGetInTouch()$"
            , signed: "$_paymentsSigned()$"
            , subscribeheader : "$_paymentsSubscribeHeader()$"
            , chooseplan : "$_paymentsChoosePlan()$"
            , paywithcard: "$_paymentsPayWithCard()$"
            , selectplan: "$_paymentPlanSelectPlan()$"
            , savechanges : "$_SaveChanges()$"
            , changeconfirm: "$_paymentPlanChangeConfirmMessage()$"
            , changeaccount: "$_paymentPlanChangeAccount()$"
            , permonth: "$_paymentPlanPerMonth()$"
            , peruserpermonth: "$_paymentPlanPerUserPerMonth()$"
            , perprocesspermonth: "$_paymentPlanPerProcessPerMonth()$"
            , perdocument: "$_paymentPlanPerDocument()$"
            , pendingline : {part1: "$_paymentPlanPendingLinePart1()$",
                             part2: "$_paymentPlanPendingLinePart2()$"}
            , savingsubscription : "$_paymentPlanSavingSubscription()$"
            , loading : "$_paymentPlanLoading()$"
            , askviktor : "$_paymentPlanAskViktor()$"
            , cancelsubscription: "$_paymentPlanCancelSubscription()$"
            , select : "$_paymentsSelect()$"
            , selected : "$_paymentsSelected()$"
            , flatrate : "$_paymentsFlatRate()$"
            , peruserrate : "$_paymentsPerUserRate()$"
            , integrations : "$_paymentsIntegrations()$"
            , enterpriseprice : "$_paymentsEnterprisePrice()$"
            , guarantee : "$_paymentsGuarantee()$"
            , nousers : "$_paymentsNoUsers()$"
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
        }  
      , documents : "$_docListTabsDocs()$"
      , templates : "$_docListTabsTemplates()$"
      , attachments : "$_docListTabsAttachemtns()$"
      , archive : {
                documents : {
                       name :  "$_docListTabsDocs()$"
                    ,  sendreminder : "$_SendReminder()$"
                    ,  remove : "$_delete()$"
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
                    ,  createnew : "$_createNewDocument()$"
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
                            , cantremoveactive : "$_CantRemoveActiveDocuments()$"
                            , successMessage : "$_flashMessageDocumentArchiveDone()$"
                            , emptyMessage : "$_noDocumentChosen()$"
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
                    ,  shared : "$_shared()$"
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
                    ,  shared : "$_pageAttachmentListIfShared()$"
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
                    ,  columns :{
                              time   : "$_SortTime()$"
                            , title  : "$_SortDoc()$"
                            , type   : "$_SortType()$"

                       }
               }
        }
      , loginModal : {
             login : "$_login()$"
           , email : "$_email()$"
           , password : "$_password()$"
           , forgotpassword : "$_forgotpassword()$"
           , sendNewPassword : "$_loginModalForgotPasswordSendNewPassword()$"
           , loginFailed : "$_flashMessageLoginPageRedirectReasonWrongMailOrPassword()$"
           , invalidEmail : "$_flashEmailIncorectlyFormed()$"
           , noUser : "$_cantSendReminderNoUser()$"
           , tooMuch : "$_cantSendReminderTooMuch()$"
           , passwordReminderSend : "$_flashMessageChangePasswordEmailSend()$"

      }
        , blocking : {
           users : {
             free     : "$_blockingUsersFree()$",
             notTeam  : "$_blockingUsersNotTeam()$",
             noneLeft : "$_blockingUsersNoneLeft()$",
             overdue  : "$_blockingUsersOverdue()$"
           },
           branding : {
             free    : "$_blockingBrandingFree()$",
             notTeam : "$_blockingBrandingNotTeam()$",
             overdue : "$_blockingBrandingOverdue()$",
             companyfree    : "$_blockingBrandingCompanyFree()$",
             companyoverdue : "$_blockingBrandingCompanyOverdue()$",
             companyNotTeam : "$_blockingBrandingCompanyProf()$"
           },
           create : {
             noneLeftFree  : "$_blockingCreateNoneLeftFree()$",
             noneLeftBlock : "$_blockingCreateNoneLeftBlock()$",
             csvfree       : "$_blockingCreateCSVFree()$",
             csvblock      : "$_blockingCreateCSVBlock()$",
             overdue       : "$_blockingCreateOverdue()$"
           },
           title : {
             free    : "$_blockingModalTitleFree()$",
             upgrade : "$_blockingModalTitleUpgrade()$",
             update  : "$_blockingModalTitleUpdate()$"
           },
           button : {
             purchase    : "$_blockingButtonPurchase()$",
             doublecheck : "$_blockingButtonDoubleCheck()$",
             upgrade     : "$_blockingButtonUpgrade()$"
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
          , grantedPriviliges: "$_apiDashboardGrantedPriviliges()$"
          , grantedPriviligesDescription: "$_apiDashboardGrantedPriviligesDescription()$"
          , personalToken: "$_apiDashboardPersonalToken()$"
          , personalTokenDescription: "$_apiDashboardpersonalTokenDescription()$"
          , personalTokenCreate: "$_apiDashboardPersonalTokenCreate()$"
          , apiToken: "$_apiDashboardApiToken()$"
          , apiTokenDescription : "$_apiDashboardApiTokenDescription()$"
          , apiTokenCreate : "$_apiDashboardApiTokenCreate()$"
          , personOrCompany : "$_apiDashboardPersonOrCompany()$"
          , privilige       :  "$_apiDashboardPrivilige()$"
      }
    };
###
