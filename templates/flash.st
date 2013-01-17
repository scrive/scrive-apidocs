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
flashMessageContractRestarted=$_flashMessageContractRestarted()$
###
flashMessageOfferRestarted=$_flashMessageOfferRestarted()$
###
flashMessageOrderRestarted=$_flashMessageOrderRestarted()$
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
flashMessageActivationLinkNotValid=
	$_flashMessageActivationLinkNotValid()$
###
flashMessageUserActivated=	$_flashMessageUserActivated()$

###
flashMessageUserAlreadyActivated=
	$_flashMessageUserAlreadyActivated()$

###
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
flashMessageValueNotValid = $_flashMessageValueNotValid()$
###
flashMessageInvalidCSV=	$_flashMessageInvalidCSV()$
###
flashMessageCSVSent=	$_flashMessageCSVSent()$
###
flashMessageUserHasBecomeCompanyAccount=$_flashMessageUserHasBecomeCompanyAccount()$
###
flashMessageUserHasLiveDocs=$_flashMessageUserHasLiveDocs()$
###
flashMessageCompanyAccountDeleted=$_flashMessageCompanyAccountDeleted()$
###
flashCompanyAccountInviteSent=$_flashCompanyAccountInviteSent()$
###
flashCompanyAccountInviteResent=$_flashCompanyAccountInviteResent()$
###
flashMessageChangeEmailMailSent=$_flashMessageChangeEmailMailSent()$
###
flashMessageMismatchedEmails=$_flashMessageMismatchedEmails()$
###
flashMessageProblemWithEmailChange=$_flashMessageProblemWithEmailChange()$
###
flashMessageYourEmailHasChanged=$_flashMessageYourEmailHasChanged()$
###
flashMessageProblemWithPassword=$_flashMessageProblemWithPassword()$
###
