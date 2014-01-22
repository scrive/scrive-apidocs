#####
##### MAILS - Separated in sections
#####
#####
#####
#####
##### MAIL AWAITING AUTHOR
#####
#####
mailDocumentAwaitingForAuthor=
    $_mailDocumentAwaitingForAuthorTitle()$
    $nicemail(
        mailheader= _mailDocumentAwaitingForAuthorTitle();
        mailp1 = mailDocumentAwaitingForAuthorText();
        mailp2 = nicebutton(link=documentlink; text=_mailReviewOnlineDocument());
        footer = legalparagraph()
    )$
#####
mailDocumentAwaitingForAuthorText=
$if(someonesigned)$
 $_mailDocumentAwaitingForAuthorContentSignedBy()$
$else$
 $_mailDocumentAwaitingForAuthorContentReadyForSigning()$
$endif$
#####
mailClosedSignup=

$if(confirmationlink)$
$_mailClosedSignup()$
<br/>
<br/>
$nicebutton(link=confirmationlink; text=_mailClosedButton())$
$else$
$_mailClosedLogin()$
$endif$
#####
##### CLOSED MAIL
#####
mailContractClosed=
    $_mailDocumentClosedTitleContract()$
    $nicemail(
        mailheader = _mailDocumentClosedTitleContract();
        mailp1 = mailContractClosedMainP();
        footer = footer
    )$

####
mailContractClosedMainP =
<br/>
    $if(sealFixed)$ $_mailDocumentCorrectionBody()$ $else$ $_mailContractClosedContentSignedBy()$ $endif$
    <br/>
    <br/>
    $_mailContractClosedContentAttached()$
    $mailClosedSignup()$
####
#### REJECTED MAIL
####
mailRejectContractMail=
      $_mailRejectMailTitleContract()$
      $nicemail(
        mailheader = _mailRejectMailTitleContract();
        mailp1 = _mailRejectMailContentRejected();
        mailspacing1 = "";
        mailp2 = mailRejectMailContentHeader();
        mailp3 = nicebutton(link=concat(a1=ctxhostpart;a2=loginlink); text=_mailRejectMailContentRejectedButton());
        footer = null
      )$
####
mailRejectMailContentHeader=
$if(customMessage)$
  <br/>
  <br/>
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <i>$customMessage:noescape()$</i>
$else$
  <div class="editable" name="custommessage"></div>
$endif$
####
#### FOREWARDING DOCUMENT
####
mailForwardSigned =
      $_mailForwardMailTitle()$
      $nicemail(
        mailheader = _mailForwardMailTitle();
        mailp1 = _mailForwardMailContent1();
        mailp2 = _mailForwardMailContent2();
        footer = null
      )$
#### INVITATION MAILS
####
mailInvitationTitleContract=
    $if(sign)$
        $_mailInvitationToSignTitleContract()$
    $elseif(send)$
        $_mailInvitationToSignTitleContract()$
    $elseif(view)$
        $_mailInvitationToViewTitleContract()$
    $endif$
###
mailInvitationToSignContract=
    $mailInvitationTitleContract()$
     $nicemail(
        mailheader= mailInvitationTitleContract();
        mailp1 = header;
        mailspacing1 = "<br/>";
        mailp2 = mailInvitationToSignContractIntructions();
        mailp3 = footer;
        footer = legalparagraph()
    )$
###
mailInvitationToSignContractIntructions=
    $nicebutton(link=link; text=_mailReviewOnlineDocument())$
####
mailInvitationToSignContractDefaultHeader=
$_mailInvitationToSignContractDefaultHeader()$
<br/>
$if(!hascustommessage)$
<div class="editable" name="customtext">
</div>
$endif$
####
mailInvitationToViewDefaultHeader=
<br/>
$_mailInvitationToViewDefaultHeader()$
<br/>
<div class="editable" name="customtext">
</div>
###
mailInvitationCustomInvitationHeader=
$defaultheader:noescape()$
<br/>
$_mailInvitationCustomInvitationHeader()$:
<br/>
<i>$custommessage:noescape()$</i>
###
### Reminding mails - SIGNED
###
###
remindMailSigned=
    $_remindMailSignedTitle()$
    $nicemail(
        mailheader= _remindMailSignedTitle();
        mailp1 = remindMailSignedP()
    )$
###
remindMailSignedP=
$header:noescape()$
####
remindMailSignedStandardHeader=
<br/>
  $_remindMailSignedContractStandardHeaderSendRequest()$
  <br/>
  <br/>
  $_remindMailSignedContractStandardHeaderSendRequest2()$
###
###
### Reminding mails - NOT SIGNED
###
remindMailNotSignedContract=
    $_remindMailNotSignedTitleContract()$

    $nicemail(
        mailheader=_remindMailNotSignedTitleContract();
        mailp1=remindMailNotSignedContractP1();
        footer= legalparagraph()
    )$
#####
remindMailNotSignedContractP1=
$if(custommessage)$
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <i>$custommessage:noescape()$</i>
$else$
      <div class="editable" name="custommessage">$_remindMailNotSignedContractBody()$</div>
$endif$
<br/>
<br/>
$nicebutton(link=link; text=_mailReviewOnlineDocument())$
###
###
###  NEW USER MAIN
###
###
newUserMail=
$_newUserMailTitle()$
$nicemail(
        mailheader=_newUserMailTitle();
        mailp1=newUserMailMainP()
    )$
####
newUserMailMainP=
$_newUserMailContentBodyFirst()$
<br/>
<br/>
$nicebutton(link=concat(a1=ctxhostpart;a2=activatelink); text=_mailActivateAccountButton())$
####
#### INVITE TO COMPANY MAIL
####
mailNewCompanyUserInvite=
    $_mailNewAccountCreatedByAdminTitle()$
    $nicemail(mailheader=_mailNewAccountCreatedByAdminTitle();mailp1=mailNewCompanyUserInviteMainP())$
####
mailNewCompanyUserInviteMainP=
 <br/>
 $_mailNewAccountCreatedByAdminContentHeaderBodyFirst()$
 <br/>
 <br/>
 $_mailNewCompanyUserInvitePrivacyInfo()$
 <br/>
 <br/>
 $_mailNewAccountCreatedByAdminContentHeaderBodySecond()$
 <br/>
 <br/>
 $_mailNewCompanyUserInviteContentBodyUserName()$ $invitedemail$
 <br/>
 <br/>
 $_mailNewCompanyUserInviteContentClickLink()$
 <br/>
 <br/>
 $nicebutton(link=concat(a1=ctxhostpart;a2=link); text=_mailNewCompanyUserInviteContentButton())$
###
### NEW ACCOUNT BY ADMIN
###
mailNewAccountCreatedByAdminContentHeader=
$if(custommessage)$
    $custommessage:noescape()$
$else$
    <br/>
    $_mailNewAccountCreatedByAdminContentHeaderBodyFirst()$
    <br/>
    <br/>
    $_mailNewAccountCreatedByAdminContentHeaderBodySecond()$
$endif$
###
mailNewAccountCreatedByAdmin=
   $_mailNewAccountCreatedByAdminTitle()$
    $nicemail(
        mailheader=_mailNewAccountCreatedByAdminTitle();
        mailp1=mailNewAccountCreatedByAdminContentHeader();
        mailp2=mailNewAccountCreatedByAdminMainP()
    )$
####
mailNewAccountCreatedByAdminMainP=
    $_mailNewAccountCreatedByAdminContentUserName()$ $email$
    <br/>
    <br/>
    $_mailNewAccountCreatedByAdminContentBody()$
    <br/>
    <br/>
    $nicebutton(link=concat(a1=ctxhostpart;a2=passwordlink); text=_mailNewAccountCreatedByAdminButton())$
####
####
invitationMailDeferred=
    $_invitationMailDeferredTitle()$
    $nicemail(
        mailheader=_invitationMailDeferredTitle();
        mailp1=invitationMailDeferredMainP()
    )$
###
invitationMailDeferredMainP=
      <br/>
      $_invitationMailDeferredContentBodyFirst()$
      <br/>
      <br/>
      $_invitationMailDeferredContentBodySecond()$
      <br/>
      <br/>
      $nicebutton(link=concat(a1=ctxhostpart;a2=unsigneddoclink); text=_invitationMailDeferredContentButton())$
###
###
invitationMailDeliveredAfterDeferred=
    $_invitationMailDeliveredAfterDeferredTitle()$
    $nicemail(
        mailheader=_invitationMailDeliveredAfterDeferredTitle();
        mailp1=_invitationMailDeliveredAfterDeferredContentBody()
    )$
###
### UNDELIVERED MAIL
###
invitationMailUndelivered=
    $_invitationMailUndeliveredTitle()$
    $nicemail(
        mailheader=_invitationMailUndeliveredTitle();
        mailp1=invitationMailUndeliveredMainP()
    )$
####
####
invitationMailUndeliveredMainP =
       <br/>
       $_invitationMailUndeliveredContentBody()$
       <br/>
       <br/>
       $_invitationMailUndeliveredContentBodyFirst()$
       <div style="margin-left: 50px;">
         $_invitationMailUndeliveredContentInstruction()$
       </div>
#######
### UNDELIVERED SMS
###
invitationSMSUndelivered=
    $_invitationSMSUndeliveredTitle()$
    $nicemail(
        mailheader=_invitationSMSUndeliveredTitle();
        mailp1=invitationSMSUndeliveredMainP()
    )$
####
####
invitationSMSUndeliveredMainP =
       <br/>
       $_invitationSMSUndeliveredContentBody()$
       <br/>
       <br/>
       $_invitationSMSUndeliveredContentBodyFirst()$
       <div style="margin-left: 50px;">
         $_invitationSMSUndeliveredContentInstruction()$
       </div>
####
#### DOCUMENT ERROR
####
mailDocumentError=
    $_mailDocumentErrorTitle()$
    $nicemail(
        mailheader=_mailDocumentErrorTitle();
        mailp1=_mailDocumentErrorContentBodyFirst();
        mailp2=_mailDocumentErrorContentBodySecond();
        mailspacing3=''
    )$
####
#### ACCESS NEW ACCOUNT MAIL
####
accessNewAccountMail=
    $_accessNewAccountMailTitle()$
    $nicemail(
        mailheader=_accessNewAccountMailTitle();
        mailp1=accessNewAccountMailMainP();
        mailp2=nicebutton(link=concat(a1=ctxhostpart;a2=passwordlink); text=_accessNewAccountMailContentButton())
    )$
###
accessNewAccountMailMainP=
<br/>
$_accessNewAccountMailContentBody()$
####
#### CHANGE PASSWORD MAIL
####
passwordChangeLinkMail=
    $_passwordChangeLinkMailTitle()$
    $nicemail(
        mailheader=_passwordChangeLinkMailTitle();
        mailp1=_passwordChangeLinkMailContentBody();
        mailp2=nicebutton(link=concat(a1=ctxhostpart;a2=passwordlink); text=_passwordChangeLinkButtonLabel());
        mailp3=_passwordChangeLinkMailContentBody2()
    )$
###
### Account takeover
###
mailTakeoverSingleUserInvite=
  $_mailTakeoverPrivateUserInviteTitle()$
  $nicemail(
      mailheader=_mailTakeoverPrivateUserInviteTitle();
      mailp1=mailTakeoverPrivateUserInviteP()
    )$
####
mailTakeoverPrivateUserInviteP=
  <br/>
  $_mailTakeoverPrivateUserInviteContentBodyFirst()$
  <br/>
  <br/>
  $_mailTakeoverPrivateUserInviteContentBodySecond()$
  <br/>
  <ol>
    $_mailTakeoverPrivateUserInviteContentInstructionSecond()$
  </ol>
  $_mailTakeoverPrivateUserInviteContentInstructionThird()$
  <br/>
  <br/>
  $nicebutton(link=concat(a1=ctxhostpart;a2=link); text=_mailTakeoverPrivateUserInviteAcceptButton())$
####
mailRequestChangeEmail=
  $_mailRequestChangeEmailTitle()$
  $nicemail(
    mailheader=_mailRequestChangeEmailTitle();
    mailp1=mailRequestChangeEmailP()
  )$
####
mailRequestChangeEmailP=
  <br/>
  $_mailRequestChangeEmailExplanation()$
  <br/>
  <br/>
  $nicebutton(link=concat(a1=ctxhostpart;a2=link); text=_mailRequestChangeEmailButton())$
####
####   UTILS
####
legalparagraph=
$_legalparagraphBody()$
####
####
####
#### DOCUMENT INFORMATION PREVIEW FOR MAIL
####
####
concat=$a1:noescape()$$a2:noescape()$$a3:noescape()$
