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
        mailp2 = nicebutton(link=documentlink; text=_mailGoToDocument());
        footer = legalparagraph()
    )$
#####
mailDocumentAwaitingForAuthorText=
$if(someonesigned)$
 $_mailDocumentAwaitingForAuthorContentSignedBy()$
$else$
 $_mailDocumentAwaitingForAuthorContentReadyForSigning()$
$endif$
<br/>
<br/>
 $_mailDocumentAwaitingForAuthorFurtherInstructions()$
#####
mailClosedCustomeMessage=
$if(custommessage)$
 $_mailClosedCustomTextHeader()$:
  <br/>
  <i>$custommessage:noescape()$</i>
<br/>
$endif$
#####
##### CLOSED MAIL
#####
mailContractClosed=
    $mailDocumentClosedTitleContract()$
    $nicemail(
        mailheader = mailDocumentClosedTitleContract();
        mailp1 = mailContractClosedMainP();
        footer = footer
    )$

####
mailDocumentClosedTitleContract=
    $if(sealFixed)$
        $_mailDocumentClosedTitleContractCorrection()$
    $else$
        $_mailDocumentClosedTitleContractConfirmation()$
    $endif$
###
mailContractClosedMainP =
    $if(sealFixed)$
      $_mailDocumentCorrectionBody()$
    $else$
      $_mailContractClosedContentSignedBy()$
    $endif$
    <br/>
    <br/>
    $if(documentAttached)$
      $_mailContractClosedContentAttached()$
      <br/>
      <br/>
      $if(custommessage)$
        $mailClosedCustomeMessage()$
        <br/>
      $endif$
    $else$
      $_mailContractClosedContentLinked()$
      $if(!hasaccount)$
        $_mailContractClosedContentLinkedNoAccount()$
        $if(custommessage)$
          <br/>
          <br/>
          $mailClosedCustomeMessage()$
        $else$
          <br/>
        $endif$
        <br/>
        $nicebutton(link=mainfilelink; text=_mailContractClosedContentLinkedNoAccountButton())$
      $else$
        $_mailContractClosedContentLinkedAccount()$
        $if(custommessage)$
          <br/>
          <br/>
          $mailClosedCustomeMessage()$
        $else$
          <br/>
        $endif$
        <br/>
        $nicebutton(link=doclink; text=_mailContractClosedContentLinkedAccountButton())$
      $endif$
    $endif$
####
#### REJECTED MAIL
####
mailRejectContractMail=
      $_mailRejectMailTitleContract()$
      $nicemail(
        mailheader = _mailRejectMailTitleContract();
        mailp1 = _mailRejectMailContentRejected();
        mailp2 = _mailRejectMailContentRejected2();
        mailspacing2 = "";
        mailp3 = mailRejectMailContentHeader();
        mailspacing3 = "";
        mailp4 = mailRejectLoginButton();
        footer = null
      )$
####
mailAuthorRejectContractMail=
      $_mailRejectMailTitleContract()$
      $nicemail(
        mailheader = _mailRejectMailTitleContract();
        mailp1 = _mailRejectMailContentRejected();
        mailspacing1 = "";
        mailp2 = mailRejectMailContentHeader();
        footer = null
      )$
####
mailRejectLoginButton=
<div class="email-preview-hidden">
  <br/>
  <br/>
  $nicebutton(link=concat(a1=ctxhostpart;a2=loginlink); text=_mailRejectMailContentRejectedButton())$
</div>
####
mailRejectMailContentHeader=
$if(customMessage)$
  <br/>
  <br/>
  $_mailRejectionCustomInvitationHeader()$:
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
        mailp2 = mailDocumentAttachedOrLinked(attached=_mailForwardMailContent2());
        footer = null
      )$
####
mailDocumentAttachedOrLinked =
 $if(documentAttached)$
   $attached$
 $else$
   $_mailDocumentLinked()$
  <br/><br/>$nicebutton(link=mainfilelink; text=_mailDocumentLinkedButton())$
 $endif$
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
        mailp1 = mailInvitationCustomHeader();
        mailspacing1 = "<br/>";
        mailp2 = mailInvitationToSignContractIntructions();
        footer = legalparagraph()
    )$
###
mailInvitationToSignContractIntructions=
    $nicebutton(link=link; text=_mailGoToDocument())$
####
mailInvitationCustomHeader=
$if(sign)$
  $_mailInvitationToSignContractDefaultHeader()$
  <br/>
  <br/>
  $_mailInvitationToSignFollowInstructions()$
  <br/>
$elseif(view)$
  $_mailInvitationToViewDefaultHeader()$
  <br/>
  <br/>
  $_mailInvitationToSignFollowInstructions()$
  <br/>
$endif$

$if(hascustommessage)$
  <br/>
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <i style="color: $nicemailfgcolour()$">$custommessage:noescape()$</i>
$endif$
### Reminding mails - SIGNED
###
###
remindMailSigned=
    $_remindMailSignedTitle()$
    $nicemail(
        mailheader= _remindMailSignedTitle();
        mailp1 = remindMailSignedStandardHeader();
        mailp2 = remindMailSignedP2()
    )$
###
remindMailSignedP2=
$if(custommessage)$
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <i>$custommessage:noescape()$</i>
  <br/>

$else$
      <div class="editable" name="custommessage"></div>
$endif$
$if(!documentAttached)$
  $if(custommessage)$
    <br/>
  $endif$
  $nicebutton(link=mainfilelink; text=_mailDocumentLinkedButton())$
$endif$
####
remindMailSignedStandardHeader=
<br/>
  $_remindMailSignedContractStandardHeaderSendRequest()$
  <br/>
  <br/>
  $if(documentAttached)$
    $_remindMailSignedContractStandardHeaderSendRequest2()$
  $else$
    $_mailDocumentLinked()$
  $endif$
###
###
### Reminding mails - NOT SIGNED
###
remindMailNotSignedContract=
    $_remindMailNotSignedTitleContract()$

    $nicemail(
        mailheader=_remindMailNotSignedTitleContract();
        mailp1=remindMailNotSignedContractBody();
        mailp2=remindMailNotSignedContractP2();
        footer= legalparagraph()
    )$
#####
remindMailNotSignedContractBody=
$_remindMailNotSignedContractBody()$
<BR/>
<BR/>
$_remindMailNotSignedContractBodyFurther()$
#####
remindMailNotSignedContractP2=
$if(custommessage)$
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <i>$custommessage:noescape()$</i>
  <br/>
$else$
      <div class="editable" name="custommessage"></div>
$endif$
$nicebutton(link=link; text=_mailGoToDocument())$
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
 $_mailNewCompanyUserInviteContentBodyUserName()$ <strong>$invitedemail$</strong>
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
  <br/>
  $_mailNewAccountCreatedByAdminContentHeaderBodyFirst()$
  <br/>
  <br/>
  $_mailNewAccountCreatedByAdminContentHeaderBodySecond()$
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
    $_mailNewAccountCreatedByAdminContentUserName()$ <strong>$email$</strong>
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
         <table>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">1.</td>
             <td style="font-family: $nicemailfont()$;">
               $_invitationMailUndeliveredContentInstructionSteps()$
               <br/>
               <div style="margin-bottom: 10px;"></div>
               $nicebutton(link=concat(a1=ctxhostpart;a2=unsigneddoclink); text=_invitationMailUndeliveredContentBodyButton())$
               <div style="margin-top: 10px;"></div>
             </td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">2.</td>
             <td style="font-family: $nicemailfont()$;">$_invitationMailUndeliveredContentInstructionStep1()$</td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">3.</td>
             <td style="font-family: $nicemailfont()$;">$_invitationMailUndeliveredContentInstructionStep2()$</td>
           </tr>
         </table>
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
         <table>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">1.</td>
             <td style="font-family: $nicemailfont()$;">
               $_invitationSMSUndeliveredContentInstructionSteps()$
               <br/>
               <div style="margin-bottom: 10px;"></div>
               $nicebutton(link=concat(a1=ctxhostpart;a2=unsigneddoclink); text=_invitationMailUndeliveredContentBodyButton())$
               <div style="margin-top: 10px;"></div>
             </td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">2.</td>
             <td style="font-family: $nicemailfont()$;">$_invitationSMSUndeliveredContentInstructionStep1()$</td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">3.</td>
             <td style="font-family: $nicemailfont()$;">$_invitationSMSUndeliveredContentInstructionStep2()$</td>
           </tr>
         </table>
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
supportmaillink=
<a href='mailto:support@scrive.com'>support@scrive.com</a>
####
accountingmaillink=
<a href='mailto:accounting@scrive.com'>accounting@scrive.com</a>
####
#### DOCUMENT INFORMATION PREVIEW FOR MAIL
####
####
concat=$a1:noescape()$$a2:noescape()$$a3:noescape()$
####

