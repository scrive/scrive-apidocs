####
#### This module puts in global context object 'localisation'
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
             , attachment : "$_Attachment()$"
             , attachmentsWord : "$_Attachments()$"
             , requestedAttachments : "$_RequestedAttachments()$"
             , send : "$_send()$"
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
                                   , body1: "$_pageDocumentForSignContentElegBody1()$"
                                   , clickHere : "$_pageDocumentForSignContentElegBody2ClickHere()$"
                                   , body2 : "$_pageDocumentForSignContentElegBody3()$"
                                 }
                    }
             , signByAuthor : {
                          modalTitle : "$_dialogConfirmSignByAuthorHead()$"
                        , modalBody1 : "$_dialogConfirmSignByAuthorBody1()$"
                        , modalBody2 : "$_dialogConfirmSignByAuthorBody2()$"
                        , modalBody3 : "$_dialogConfirmSignByAuthorBody3()$"
                        , eleg : {
                                     subhead : "$_dialogConfirmSignByAuthorElegSubHead()$"
                                   , body1: "$_dialogConfirmSignByAuthorElegBody()$"
                                   , clickHere : "$_dialogConfirmSignByAuthorElegClickHere1()$"
                                   , body2 : "$_dialogConfirmSignByAuthorElegClickHere2()$"
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
            , step2template : "$_step2selectIfTemplate()$"
            , step2basic    : "$_step2selectIfisbasic()$"
            , step2normal   : "$_step2selectIfElse()$"
            , step3template : "$_step3selectIfTemplate()$"
            , step3normal   : "$_step3selectIfNotTemplate()$"
            , saveAsTemplate : "$_signStepsMenuEmptyStepSaveAsTemplate()$"
            , saveTemplate : "$_signStepsBodyFunctionssaveTemplate()$"
            , switchToAdvanced : {
                      option: "$_signStepsFunctionalityChangeToAdvanced()$"
                    , title : "$_dialogConfirmAdvancedChangeToHead()$"
                    , content : "$_dialogConfirmAdvancedBody()$"
                    , button : "$_dialogConfirmAdvancedChangeToFooterChangeProcess()$"
                    }
            , skrivapa : "$_skrivapa()$"


            , tweetAccount : "$_tweetAccount()$"
            , selectFileToUpload : "$_selectFileToUpload()$"
            , onlyFileWithTypeAllowed : function(t) {
                return "$_onlyFileWithTypeAllowed1()$" + t + "$_onlyFileWithTypeAllowed2()$"
              }
            , fileTooLarge: "$_fileTooLarge()$"
            , couldNotUpload: "$_couldNotUpload()$"
            , process : {
                contract: {
                   uploadprompt: "$_contractuploadprompttext()$",
                   name: "$_contractname()$"
                },
                offer: {
                   uploadprompt: "$_offeruploadprompttext()$",
                   name: "$_offername()$"
                },
                order: {
                   uploadprompt: "$_orderuploadprompttext()$",
                   name: "$_ordername()$"
                }

              }
           , searchTemplate : "$_SearchTemplate()$"
           , sortTemplate : "$_SortTemplate()$"
           , infoSelectTemplate : "$_InfoSelectTemplate()$"
           , goBack : "$_GoBack()$"
           , verification : {
                    selectmethod: "$_signStepsBodyFunctionsBoxChooseVerification()$"
                }
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
           , attachments: {
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
              validation : {
                      missingOrWrongEmail : "$_emptyOrNotValidEmail()$"
                    , missingOrWrongNames:  "$_missingSignatoryNames()$"
                    , missingOrWrongPersonalNumber : "$_backToStepTwoAndFillPersonNumber()$"
                    , missingOrWrongPlacedAuthorField : "$_missingPlacedAuthorFieldValue()$"
                    , missingOrWrongCustomFieldValue: "$_missingCustomFieldValue()$"
                    , sameMails : "$_repetedEmailAddress()$"
                    , atLeastOnePersonOtherThenAuthor : "$_atLeastOnePersonOtherThenAuthor()$"
                    , notReadyField : "$_setAllFields()$"
                    , notPlacedField : "$_placeAllFields()$"
                    , restrictedName : "$_fieldnameRestricted()$"
                    , notPlacedSignature : "Signature must be placed for signing party with pad authorization"
                }
             , messages : {
                      sendingDocument: "$_designViewMessageSending()$"
                    , reloadingDocument : "$_designViewMessageReloading()$"
                    , extraActions : "$_designViewMessageExtraActions()$"
                }
             , sign : "$_sign()$"
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
                authorization : "$_padAuthorization()$"
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
             ,  addToPadQueueFor :  "$_padAddToPadQueueFor()$"
             ,  cleanImage : "$_padCleanImage()$"
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
         , docupload : {
             choosefile: "$_signStepsBodyUploadBoxStep1UpLoadChooseFile()$"
            ,choosetemplate: "$_signStepsBodyUploadBoxStep1ChooseTemplate()$"
            ,templatesaved: "$_signStepsBodyUploadBoxStep1TemplateSaved()$"
            ,uploadinfo: "$_contractuploadprompttext()$"
         }
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
        }
        , poweredByScrive : "$_poweredByScrive()$"
    }

####
