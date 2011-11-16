### Some comment here
flashDocumentDraftSaved=
	$_flashDocumentDraftSaved()$

######
flashDocumentTemplateSaved=
	$_flashDocumentTemplateSaved()$

######
flashRemindMailSentNotSigned=
	$_flashRemindMailSentNotSigned()$ $personname$

######
flashRemindMailSentSigned=
	$_flashRemindMailSentSigned()$ $personname$

######
flashMessageCannotCancel=
	$_flashMessageCannotCancel()$

######
flashMessageContractCanceled=
     Du har nu återkallat inbjudan att underteckna dokumentet och ett meddelande om återkallelsen har skickats till samtliga parter.
######
flashMessageOfferCanceled=
     Du har nu återkallat offerten och ett meddlande om återkallelsen har skickats till samtliga parter
#####
flashMessageOrderCanceled=
     Du har nu återkallat ordern och ett meddelande om återkallelsen har skickats till samtliga parter.
#####
flashMessageNoAccountType=
	$_flashMessageNoAccountType()$
#####
flashMessageInvalidAccountType=
	$_flashMessageInvalidAccountType()$
#####
flashMessageMustAcceptTOS=
	$_flashMessageMustAcceptTOS()$

####
flashMessageUserDetailsSaved=
	<div>$_flashMessageUserDetailsSaved()$</div>
####
flashMessageCompanyCreated=
        <div>$_flashMessageCompanyCreated()$</div>
####
flashMessageBadOldPassword=
	$_flashMessageBadOldPassword()$

####
flashMessagePasswordsDontMatch=
	$_flashMessagePasswordsDontMatch()$

###
flashMessageContractRestarted=
    Du har startat om dokumentet och kan nu underteckna och skicka inbjudan på nytt.
###
flashMessageOfferRestarted=
    Du har startat om offerten och kan nu skicka den på nytt
###
flashMessageOrderRestarted=
    Du har startat om orderbekräftelsen och kan nu skicka den på nytt
###
flashMessageUserPasswordChanged=
	$_flashMessageUserPasswordChanged()$

###
flashMessagePasswordChangeLinkNotValid=
	$_flashMessagePasswordChangeLinkNotValid()$
###
flashMessageUserWithSameEmailExists=
	$_flashMessageUserWithSameEmailExists()$

###
flashMessageViralInviteSent=
	$_flashMessageViralInviteSent()$

###
flashMessageOtherUserSentInvitation=
	$_flashMessageOtherUserSentInvitation()$
###
flashMessageNoRemainedInvitationEmails=
	$_flashMessageNoRemainedInvitationEmails()$
###
flashMessageActivationLinkNotValid=
	$_flashMessageActivationLinkNotValid()$
###
flashMessageUserActivated=	$_flashMessageUserActivated()$

###
flashMessageUserAlreadyActivated=
	$_flashMessageUserAlreadyActivated()$

###
flashMessageChangePasswordEmailSend=
	$_flashMessageChangePasswordEmailSend()$
###
flashMessageNoRemainedPasswordReminderEmails=
	$_flashMessageNoRemainedPasswordReminderEmails()$
####
flashMessageNewActivationLinkSend=
	$_flashMessageNewActivationLinkSend()$

###
flashMessageUserSignupDone=	$_flashMessageUserSignupDone()$
###
flashAuthorSigned=	$_flashAuthorSigned()$
###
flashMessageLoginPageRedirectReason=
$if(notlogged)$
	$_flashMessageLoginPageRedirectReasonNotLoggedIn()$
$endif$
$if(notsu)$
	$_flashMessageLoginPageRedirectReasonNotAdmin()$
$endif$
$if(invloginfo)$
	$_flashMessageLoginPageRedirectReasonWrongMailOrPassword()$
$endif$
###
###
emailFieldName=	$_email()$
###
passwordFieldName=	$_passwordFieldName()$
###
nameFieldName=	$_nameFieldName()$
###
firstNameFieldName=	$_firstname()$
###
secondNameFieldName=	$_lastname()$
###
companyNameFieldName=	$_company()$
###
companyNumberFieldName=	$_orgnumber()$
###
addressFieldName=	$_addressFieldName()$
###
positionFieldName=	$_companyposition()$
###
checkBoxFieldName=	$_checkBoxFieldName()$
###
daysToSignFieldName=	$_daysToSignFieldName()$
###
idFieldName=	$_idFieldName()$
###
fieldNameFieldName=	$_fieldNameFieldName()$
###
fieldValueFieldName=	$_fieldValueFieldName()$
###
placeFieldName=	$_placeFieldName()$
###
inviteTextFieldName=	$_inviteTextFieldName()$
###
flashMessageNeedsLetterAndDigit=	$_flashMessageNeedsLetterAndDigit()$
###
flashMessageInvalidFormat=	$_flashMessageInvalidFormat()$
###
flashMessageNumberBelowMinimum=	$_flashMessageNumberBelowMinimum()$
###
flashMessageNumberAboveMaximum=	$_flashMessageNumberAboveMaximum()$
###
flashMessageNotAValidInteger=	$_flashMessageNotAValidInteger()$
###
flashMessageInvalidSpaceInInput=	$_flashMessageInvalidSpaceInInput()$
###
flashMessageInvalidUnprintableCharsInInput= 	$_flashMessageInvalidUnprintableCharsInInput()$
###
flashMessageInvalidPrintableCharsInInput=	$_flashMessageInvalidPrintableCharsInInput()$
###
flashMessageInputLessThanMinLength=	$_flashMessageInputLessThanMinLength()$
###
flashMessageInputExceedsMaxLength=	$_flashMessageInputExceedsMaxLength()$
###
flashMessageMissingRequiredField=	$_flashMessageMissingRequiredField()$
###
flashMessageFailedToParseCSV=	$_flashMessageFailedToParseCSV()$
###
flashMessageCSVHasTooManyRows=	$_flashMessageCSVHasTooManyRows()$
###
flashMessageNoDataInCSV=	$_flashMessageNoDataInCSV()$
###
flashMessageNoDataInCSVApartFromHeader=	$_flashMessageNoDataInCSVApartFromHeader()$
###
flashMessageRowLessThanMinColCount=	$_flashMessageRowLessThanMinColCount()$
###
flashMessageRowGreaterThanMaxColCount=	$_flashMessageRowGreaterThanMaxColCount()$
###
flashMessageFirstNameIsRequired=	$_flashMessageFirstNameIsRequired()$
###
flashMessageSecondNameIsRequired=	$_flashMessageSecondNameIsRequired()$
###
flashMessageEmailIsRequired=	$_flashMessageEmailIsRequired()$
###
flashMessageNumberIsRequired=	$_flashMessageNumberIsRequired()$
###
flashMessageBulkContractRemindsSent=	$_flashMessageBulkContractRemindsSent()$
###
flashMessageNoBulkContractRemindsSent=	$_flashMessageNoBulkContractRemindsSent()$
###
flashMessageBulkOfferRemindsSent=	$_flashMessageBulkOfferRemindsSent()$
###
flashMessageNoBulkOfferRemindsSent=	$_flashMessageNoBulkOfferRemindsSent()$
###
flashMessageBulkOrderRemindsSent=Påminnelse har skickats till alla parter som ännu inte bekräftat.
###
flashMessageNoBulkOrderRemindsSent=Tyvärr, det finns inga parter som återstår att bekräfta.
###
flashMessageContractArchiveDone=	$_flashMessageContractArchiveDone()$
###
flashMessageOfferArchiveDone=	$_flashMessageOfferArchiveDone()$
###
flashMessageOrderArchiveDone=Valda orderbekräftelse har blivit raderade.
###
flashMessageTemplateArchiveDone=	$_flashMessageTemplateArchiveDone()$
###
flashMessageAttachmentArchiveDone=	$_flashMessageAttachmentArchiveDone()$
###
flashMessageRubbishRestoreDone= $_flashMessageRubbishRestoreDone()$
###
flashMessageRubbishHardDeleteDone= $_flashMessageRubbishHardDeleteDone()$
###
flashMessageThanksForTheQuestion=	$_flashMessageThanksForTheQuestion()$
###
flashMessageWeWillCallYouSoon=$_flashMessageWeWillCallYouSoon()$
###
flashMessageInvalidCSV=	$_flashMessageInvalidCSV()$
###
flashMessageCSVSent=	$_flashMessageCSVSent()$
###
flashMessageSingleTemplateShareDone=	$_flashMessageSingleTemplateShareDone()$
###
flashMessageMultipleTemplateShareDone=	$_flashMessageMultipleTemplateShareDone()$
###
flashMessageSingleAttachmentShareDone=	$_flashMessageSingleAttachmentShareDone()$
###
flashMessageMultipleAttachmentShareDone=	$_flashMessageMultipleAttachmentShareDone()$
###
flashMessageUserHasBecomeCompanyAccount=$_flashMessageUserHasBecomeSubaccount()$
###
flashMessageAccountActivatedFromSign=	$_flashMessageAccountActivatedFromSign()$
###
flashMessageAccountRemovedFromSign=	$_flashMessageAccountRemovedFromSign()$
###
flashMessageOnlyHaveRightsToViewDoc=	$_flashMessageOnlyHaveRightsToViewDoc()$
###
flashMessagePleaseSignWithEleg=	$_flashMessagePleaseSignWithEleg()$
###
flashMessagePleaseSignContract=	$_flashMessagePleaseSignContract()$
###
flashMessagePleaseSignOffer=	$_flashMessagePleaseSignOffer()$
###
flashMessagePleaseSignOrder=Du kan bekräfta ordern längst ned på sidan.
###
flashMessageUserHasLiveDocs=	$_flashMessageUserHasLiveDocs()$
###
flashMessageAccountDeleted=	$_flashMessageAccountDeleted()$
###
flashCompanyAccountInviteSent=$_flashCompanyAccountInviteSent()$
###
