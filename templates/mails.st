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
    $_mailContractClosedContentAttached()$ $mailClosedSignup()$
####
mailOfferClosed=
    $_mailDocumentClosedTitleOffer()$
    $nicemail(
        mailheader = _mailDocumentClosedTitleOffer();
        mailp1 = mailContractClosedMainP();
        footer = footer
    )$
####
mailOrderClosed=
    $_mailDocumentClosedTitleOrder()$
    $nicemail(
        mailheader = _mailDocumentClosedTitleOrder();
        mailp1 = mailContractClosedMainP();
        footer = footer
    )$
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
        footer = null
      )$
####
mailRejectOfferMail=
      $_mailRejectMailTitleOffer()$
      $nicemail(
        mailheader = _mailRejectMailTitleOffer();
        mailp1 = _mailRejectMailContentRejected();
        mailspacing1 = "";
        mailp2 = mailRejectMailContentHeader();
        footer = null
      )$
###
mailRejectOrderMail=
      $_mailRejectMailTitleOrder()$
      $nicemail(
        mailheader = _mailRejectMailTitleOrder();
        mailp1 = _mailRejectMailContentRejected();
        mailspacing1 = "";
        mailp2 = mailRejectMailContentHeader();
        footer = null
      )$

###
mailRejectMailContentHeader=
$if(customMessage)$
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <i>$customMessage:noescape()$</i>
  <br/>
$else$
  <div class="editable" name="custommessage"></div>
$endif$
####
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
mailInvitationTitleOffer=
    $if(sign)$
        $_mailInvitationToSignTitleOffer()$
    $elseif(send)$
        $_mailInvitationToSignTitleOffer()$
    $elseif(view)$
        $_mailInvitationToViewTitleOffer()$
    $endif$
###
mailInvitationTitleOrder=
    $if(sign)$
        $_mailInvitationToSignTitleOrder()$
    $elseif(send)$
        $_mailInvitationToSignTitleOrder()$
    $elseif(view)$
        $_mailInvitationToViewTitleOrder()$
    $endif$
###
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
mailInvitationToSignOffer=
     $mailInvitationTitleOffer()$
     $nicemail(
        mailheader= mailInvitationTitleOffer();
        mailp1 = header;
        mailspacing1 = "<br/>";
        mailp2 = mailInvitationToSignOfferIntructions();
        mailp3 = footer;
        footer = legalparagraph()
    )$
###
mailInvitationToSignOfferIntructions=
    $nicebutton(link=link; text=_mailReviewOnlineDocument())$
###
mailInvitationToSignOrder=
    $mailInvitationTitleOrder()$
    $nicemail(
        mailheader= mailInvitationTitleOrder();
        mailp1 = header;
        mailspacing1 = "<br/>";
        mailp2 = mailInvitationToSignOrderIntructions();
        mailp3 = footer;
        footer = legalparagraph()
    )$
####
mailInvitationToSignOrderIntructions=
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
mailInvitationToSignOfferDefaultHeader=
$_mailInvitationToSignOfferDefaultHeader()$
<br/>
$if(!hascustommessage)$
<div class="editable" name="customtext">
</div>
$endif$
####
mailInvitationToSignOrderDefaultHeader=
$_mailInvitationToSignOrderDefaultHeader()$
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
#####
remindMailNotSignedOffer=
    $_remindMailNotSignedTitleOffer()$
    $nicemail(
        mailheader=_remindMailNotSignedTitleOffer();
        mailp1=remindMailNotSignedOfferP1();
        footer= legalparagraph()
    )$
###
remindMailNotSignedOfferP1=
$if(custommessage)$
  $custommessage:noescape()$
$else$
      <div class="editable" name="custommessage">$_remindMailNotSignedOfferBody()$</div>
$endif$
<br/>
<br/>
$nicebutton(link=link; text=_mailReviewOnlineDocument())$
###
remindMailNotSignedOrder=
    $_remindMailNotSignedTitleOrder()$
    $nicemail(
        mailheader=_remindMailNotSignedTitleOrder();
        mailp1=remindMailNotSignedOrderP1();
        footer= legalparagraph()
    )$
###
remindMailNotSignedOrderP1=
$if(custommessage)$
  $custommessage:noescape()$
$else$
      <div class="editable" name="custommessage">$_remindMailNotSignedOrderBody()$</div>
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
 $_mailNewCompanyUserInviteContentClickLink()$ <a href="$ctxhostpart$$link$">$ctxhostpart$$link$</a>

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
    $_mailNewAccountCreatedByAdminContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a>
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
      $_invitationMailDeferredContentBodySecond()$ <a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a>
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
####
#### DOCUMENT ERROR
####
mailDocumentError=
    $_mailDocumentErrorTitle()$
    $nicemail(
        mailheader=_mailDocumentErrorTitle();
        mailp1=_mailDocumentErrorContentBodyFirst();
        mailp2=_mailDocumentErrorContentBodySecond();
        mailp3=_mailDocumentErrorContentBodyThird();
        mailspacing3=''
    )$
####
#### ACCESS NEW ACCOUNT MAIL
####
accessNewAccountMail=
    $_accessNewAccountMailTitle()$
    $nicemail(
        mailheader=_accessNewAccountMailTitle();
        mailp1=accessNewAccountMailMainP()
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
        mailp1=passwordChangeLinkMailMainP();
        mailp2=""
    )$
###
passwordChangeLinkMailMainP=
<br/>
$_passwordChangeLinkMailContentBody()$
<br/>
<br/>
$_passwordChangeLinkMailContentBody2()$
###
### MISMATCH MAILS
###
mailMismatchSignatory=
$_mailMismatchSignatoryTitle()$
    $nicemail(
        mailheader=_mailMismatchSignatoryTitle();
        mailp1=mailMismatchSignatoryMainP()
    )$
#####
mailMismatchSignatoryMainP=
<br/>
$_mailMismatchSignatoryContentBodyFirst()$
<br/>
<br/>
$_mailMismatchSignatoryContentBodySecond()$
<br/>
<br/>
$_mailMismatchSignatoryContentBodyThird()$
####
mailMismatchAuthor=
$_mailMismatchAuthorTitle()$
$nicemail(
        mailheader=_mailMismatchAuthorTitle();
        mailp1=mailMismatchAuthorMainP()
    )$
####
mailMismatchAuthorMainP =
<br/>
$_mailMismatchAuthorContentBodyFirst()$
<br/>
<br/>
$_mailMismatchAuthorContentBodySecond()$
<br/>
<br/>
$_mailMismatchAuthorContentBodyThird()$
####
####
mailTakeoverPrivateUserInvite=
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
  <br/>
  $_mailTakeoverPrivateUserInviteContentInstructionFirst()$
  <ol>
    $_mailTakeoverPrivateUserInviteContentInstructionSecond()$
  </ol>
  $_mailTakeoverPrivateUserInviteContentLink()$ $_mailTakeoverPrivateUserInviteContentInstructionThird()$
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
  <a href="$ctxhostpart$$link$">
    $ctxhostpart$$link$
  </a>
####
#### Mail API EMAILS
####
mailMailAPIConfirmContract=
   $_mailMailAPIConfirmTitle()$
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=_mailMailAPIConfirm()
    )$
####
mailMailAPIConfirmOrder=
   $_mailMailAPIConfirmTitle()$
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=_mailMailAPIConfirm()
    )$
####
mailMailAPIConfirmOffer=
   $_mailMailAPIConfirmTitle()$
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=_mailMailAPIConfirm()
    )$
####
####
####   UTILS
####
####
timetosigninfo=
    <span>$_timetosigninfo()$ <strong>$time$</strong>.</span><br/>
###
legalparagraph=
$_legalparagraphBody()$
####
####
####
#### DOCUMENT INFORMATION PREVIEW FOR MAIL
####
####
####
mailMailAPIError=
    Scrive by Mail Error
    $nicemail(
        mailheader= "Scrive by Mail Error";
        mailp1 = "<strong>Greetings from Scrive by Mail,</strong>";
        mailp2 = "We were unable to process the mail you sent us and therefore have not forwarded the document to anyone.";
        mailp3 = errormsg;
        mailp4 = "Thanks for using Scrive by Mail"
    )$
#####
linkk=<a href="$href$">$text$</a>
####
concat=$a1:noescape()$$a2:noescape()$$a3:noescape()$
