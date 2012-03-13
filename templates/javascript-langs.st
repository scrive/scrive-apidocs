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
                          guardCBoxForAuthorLabel : "$_signGuardField1()$"
                        , guardCBoxForAuthorAfterLabel : "$_signGuardField2()$"
                        , modalTitle : "$_dialogConfirmSignByAuthorHead()$"
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
                    }
            , authorAttachmentBoxHeader : "$_signViewNotsignedbymeAttachmentBoxHead()$"
            , signatoryAttachmentBoxHeader : "$_signViewSigAttachmentBoxHead()$"
            , signatoryAttachmentUploadButton: "$_signViewSigAttachmentBoxChooseFile()$"
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
              , changeEnglishToSwedishText :  "$_signInEnglishConfirmText()$"
              , signInSwedish : "$_signInSwedish()$"
              , changeSwedishToEnglishText: "$_signInSwedishConfirmText()$"
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
           }
         , designview : {
             validation : {
                      missingOrWrongEmail : "$_emptyOrNotValidEmail()$"
                    , missingOrWrongNames:  "$_missingSignatoryNames()$"
                    , missingOrWrongPersonalNumber : "$_backToStepTwoAndFillPersonNumber()$"
                    , sameMails : "$_repetedEmailAddress()$"
                    , atLeastOnePersonOtherThenAuthor : "$_atLeastOnePersonOtherThenAuthor()$"
                    , notReadyField : "$_setAllFields()$"
                    , restrictedName : "$_fieldnameRestricted()$"
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
         , sampleEmailHeader: "$_SampleEmailHeader()$"
         , sampleEmailSubheader: "$_mailsubheader()$"
         , sampleEmailContent: "$_SampleEmailContent()$"
         , changeEmail : "$_changeEmail()$"
         , onlyPDF : "$_onlyPDF()$"
    }
####
