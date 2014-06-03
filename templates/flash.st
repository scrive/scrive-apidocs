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
flashMessageUserHasBecomeCompanyAccount=$_flashMessageUserHasBecomeCompanyAccount()$
###
flashMessageProblemWithEmailChange=$_flashMessageProblemWithEmailChange()$
###
flashMessageYourEmailHasChanged=$_flashMessageYourEmailHasChanged()$
###
flashMessageProblemWithPassword=$_flashMessageProblemWithPassword()$
###
flashMessageBecomeCompanyLogInDifferentUser=$_flashMessageBecomeCompanyLogInDifferentUser()$
####
