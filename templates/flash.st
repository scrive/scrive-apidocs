######
flashRemindMailSentNotSigned=
	$_flashRemindMailSentNotSigned()$ $personname$

######
flashRemindMailSentSigned=
	$_flashRemindMailSentSigned()$ $personname$

#####
flashMessageMustAcceptTOS=
	$_flashMessageMustAcceptTOS()$

####
flashMessageUserDetailsSaved=
	<div>$_flashMessageUserDetailsSaved()$</div>
####
flashMessagePasswordsDontMatch=
	$_flashMessagePasswordsDontMatch()$

###
flashMessageContractRestarted=$_flashMessageContractRestarted()$
###
flashMessageContractProlonged=$_flashMessageContractProlonged()$
###
flashMessageUserPasswordChanged=
	$_flashMessageUserPasswordChanged()$

###
flashMessagePasswordChangeLinkNotValid=
	$_flashMessagePasswordChangeLinkNotValid()$
###
flashMessageAccessNewAccountLinkNotValid=
	$_flashMessageAccessNewAccountLinkNotValid()$
###
flashMessageUserWithSameEmailExists=
	$_flashMessageUserWithSameEmailExists()$

###
flashMessageUserActivated=	$_flashMessageUserActivated()$

###
flashMessageNewActivationLinkSend=
	$_flashMessageNewActivationLinkSend()$

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
flashMessageUserHasBecomeCompanyAccount=$_flashMessageUserHasBecomeCompanyAccount()$
###
flashMessageProblemWithEmailChange=$_flashMessageProblemWithEmailChange()$
###
flashMessageYourEmailHasChanged=$_flashMessageYourEmailHasChanged()$
###
flashMessageProblemWithPassword=$_flashMessageProblemWithPassword()$
###
