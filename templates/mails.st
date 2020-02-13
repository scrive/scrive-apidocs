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
  <p>$custommessage:br()$</p>
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
    $if(!mainfilelink)$
      $_mailContractClosedContentAttached()$
      <br/>
      <br/>
      $if(custommessage)$
        $mailClosedCustomeMessage()$
        <br/>
      $endif$
    $else$
      $if(forceLink)$
        $_mailContractClosedContentLinked()$
      $else$
        $_mailContractClosedContentLinkedTooBig()$
      $endif$
      $if(hasaccount)$
        $_mailContractClosedContentLinkedAccount()$
      $else$
        $_mailContractClosedContentLinkedNoAccount()$
      $endif$
      $if(custommessage)$
        <br/>
        <br/>
        $mailClosedCustomeMessage()$
      $else$
        <br/>
      $endif$
      <br/>
      $nicebutton(link=doclink; text=_mailContractClosedContentLinkedButton())$
    $endif$
####
mailDocumentCorrectionBodyWikiURL=
<a href='http://en.wikipedia.org/wiki/Digital_signature'>http://en.wikipedia.org/wiki/Digital_signature</a>
####
#### FORWARD SIGNING MAIL
####
mailForwardSigningForAuthorMail=
      $_mailForwardSigningForAuthorTitle()$
      $nicemail(
        mailheader = _mailForwardSigningForAuthorHeader();
        mailp1 = mailForwardSigningForAuthorContent();
        mailspacing2 = "";
        mailp2 = _mailForwardSigningForAuthorButton();
        mailp3 = mailForwardSigningForAuthorButton();
        footer = null
      )$
###
###
mailForwardSigningForAuthorContent=
$if(signing)$
  $_mailForwardSigningForAuthorContent()$
$else$
  $_mailForwardSigningForAuthorContentApproving()$
$endif$
###
###
mailForwardSigningForAuthorButton=
<div>
  $nicebutton(link=concat(a1=ctxhostpart;a2=loginlink); text=_mailGoToDocument())$
</div>
####
####
mailForwardSigningForNewSignatory=
$_mailForwardSigningForNewSignatoryTitle()$
$nicemail(
    mailp1 = mailForwardSigningForNewSignatoryDescription();
    mailspacing1 = "<br/>";
    mailp2 = mailInvitationToSignContractIntructions()
)$
####
mailForwardSigningForNewSignatoryDescription=
$if(signing)$
  $_mailForwardSigningForNewSignatoryDescriptionBase()$
$else$
  $_mailForwardSigningForNewSignatoryDescriptionBaseApproving()$
$endif$
<br/>

$if(hasforwardmessage)$
  <br/>
  $_mailForwardMessageDescription()$:
  <br/>
  <p>$forwardmessage:br()$</p>
$endif$

$if(hasinvitationmessage)$
  <br/>
  $_mailInvitationMessageDescription()$:
  <br/>
  <p>$invitationmessage:br()$</p>
$endif$
###
###
mailForwardSigningForNewSignatoryButton=
<div>
  $nicebutton(link=concat(a1=ctxhostpart;a2=link); text=_mailForwardSigningForNewSignatoryButton())$
</div>
####
####
#### PORTAL MAILS
####
mailPortalInviteWithUser=$mailPortalInvite()$
####
mailPortalInviteWithoutUser=$mailPortalInvite()$
####
mailPortalInvite=
$_mailPortalInviteTitle()$
$nicemail(
    mailp1 = _mailPortalClickToSign();
    mailspacing1 = "<br/><br/>";
    mailp2 = nicebutton(link=link; text=_mailPortalSignDocumentsButton());
    mailspacing2 = "<br/><br/>";
    mailp3 = _mailPortalIfNoAccout();
    mailspacing3 = "<br/><br/>";
    mailp4 = _mailPortalIfDontWantToSign()
)$
####
mailPortalRemindWithUser=$mailPortalRemind()$
####
mailPortalRemindWithoutUser=$mailPortalRemind()$
###
mailPortalRemind=
$_mailPortalRemindTitle()$
$nicemail(
    mailp1 = _mailPortalRemindContent();
    mailspacing1 = "<br/><br/>";
    mailp2 = nicebutton(link=link; text=_mailPortalSignDocumentsButton())
)$
####
####
#### TIMEDOUT MAIL
####
####
mailTimedoutMail=
    $_mailTimedoutTitle()$
    $nicemail(
        mailheader=_mailTimedoutTitle();
        mailp1=mailTimedoutMailMainP()
    )$
####
mailTimedoutMailMainP=
       $_mailTimedoutInstructionDescription()$
       <div style="margin-left: 50px;">
         <table>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">1.</td>
             <td style="font-family: $nicemailfont()$;">
               $_mailTimedoutInstructionStep1()$
               <br/>
               <div style="margin-bottom: 10px;"></div>
               $nicebutton(link=concat(a1=ctxhostpart;a2=documentlink); text=_mailTimedoutInstructionButton())$
               <div style="margin-top: 10px;"></div>
             </td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">2.</td>
             <td style="font-family: $nicemailfont()$;">$_mailTimedoutInstructionStep2()$</td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">3.</td>
             <td style="font-family: $nicemailfont()$;">$_mailTimedoutInstructionStep3()$</td>
           </tr>
           <tr>
             <td valign="top" style="font-family: $nicemailfont()$;">4.</td>
             <td style="font-family: $nicemailfont()$;">$_mailTimedoutInstructionStep4()$</td>
           </tr>
         </table>
       </div>
####
####
#### REJECTED MAIL
####
mailAuthorRejectContractMail=
      $_mailRejectMailTitleContract()$
      $nicemail(
        mailheader = _mailRejectMailTitleContract();
        mailp1 = mailRejectMailContentRejected();
        mailp2 = _mailRejectMailContentRejected2();
        mailspacing2 = "";
        mailp3 = mailRejectMailContentHeader();
        mailspacing3 = "";
        mailp4 = mailRejectLoginButton();
        footer = null
      )$
####
mailRejectContractMail=
      $_mailRejectMailTitleContract()$
      $nicemail(
        mailheader = _mailRejectMailTitleContract();
        mailp1 = mailRejectMailContentRejected();
        mailspacing1 = "";
        mailp2 = mailRejectMailContentHeader();
        footer = null
      )$
####
mailRejectMailContentRejected=
$if(rejectorIsApprover)$
  $_mailRejectMailContentRejectedByApprover()$
$else$
  $_mailRejectMailContentRejected()$
$endif$
####
mailRejectLoginButton=
<div class="email-preview-hidden">
  <br/>
  <br/>
  $nicebutton(link=concat(a1=ctxhostpart;a2=loginlink); text=_mailRejectMailContentRejectedButton())$
</div>
####
mailRejectMailContentHeader=
$if(hascustommessage)$
  <br/>
  <br/>
  $_mailRejectionCustomInvitationHeader()$:
  <br/>
  <span>$custommessage:br()$</span>
$else$
  <div class="editable" name="custommessage"></div>
$endif$
####
#### NOTIFICATIONS
####
mailDocumentSignedNotification =
      $_mailDocumentSignedNotificationTitle()$
      $nicemail(
        mailheader = _mailDocumentSignedNotificationTitle();
        mailp1 = _mailDocumentSignedNotificationContent();
        mailp2 = nicebutton(link=link; text=_mailGoToDocument());
        footer = legalparagraph()
      )$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToViewGoToActionName();
          actiondescription=_mailInvitationToViewGoToActionDescription()
      )$
####
mailDocumentSignedNotificationWithConfirmation =
      $_mailDocumentSignedNotificationTitle()$
      $nicemail(
        mailheader = _mailDocumentSignedNotificationTitle();
        mailp1 = _mailDocumentSignedNotificationContent();
        mailp2 = _mailDocumentPartyProcessFinalizedWithConfirmationContent();
        mailp3 = nicebutton(link=link; text=_mailGoToDocument());
        footer = legalparagraph()
      )$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToViewGoToActionName();
          actiondescription=_mailInvitationToViewGoToActionDescription()
      )$
####
mailDocumentApprovedNotification =
      $_mailDocumentApprovedNotificationTitle()$
      $nicemail(
        mailheader = _mailDocumentApprovedNotificationTitle();
        mailp1 = _mailDocumentApprovedNotificationContent();
        mailp2 = nicebutton(link=link; text=_mailGoToDocument());
        footer = legalparagraph()
      )$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToViewGoToActionName();
          actiondescription=_mailInvitationToViewGoToActionDescription()
      )$
####
mailDocumentApprovedNotificationWithConfirmation =
      $_mailDocumentApprovedNotificationTitle()$
      $nicemail(
        mailheader = _mailDocumentApprovedNotificationTitle();
        mailp1 = _mailDocumentApprovedNotificationContent();
        mailp2 = _mailDocumentPartyProcessFinalizedWithConfirmationContent();
        mailp3 = nicebutton(link=link; text=_mailGoToDocument());
        footer = legalparagraph()
      )$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToViewGoToActionName();
          actiondescription=_mailInvitationToViewGoToActionDescription()
      )$
####
#### FORWARDING DOCUMENT
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
 $if(!mainfilelink)$
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
    $elseif(approve)$
        $_mailInvitationToApproveTitleContract()$
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
    $mailInvitationToSignContractGoToActionMarkup()$
###
mailInvitationToSignContractGoToActionMarkup=
    $if(sign)$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToSignGoToActionName();
          actiondescription=_mailInvitationToSignGoToActionDescription()
      )$
    $elseif(send)$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToSignGoToActionName();
          actiondescription=_mailInvitationToSignGoToActionDescription()
      )$
    $elseif(approve)$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToApproveGoToActionName();
          actiondescription=_mailInvitationToApproveGoToActionDescription()
      )$
   $elseif(view)$
      $gmailGoToActionMarkup(
          actionurl=link;
          actionname=_mailInvitationToViewGoToActionName();
          actiondescription=_mailInvitationToViewGoToActionDescription()
      )$
    $endif$
###
mailInvitationToSignContractIntructions=
    $nicebutton(link=link; text=_mailGoToDocument())$
####
mailInvitationCustomHeader=
$if(sign)$
  $_mailInvitationToSignContractDefaultHeader()$
  <br/>
$elseif(approve)$
  $_mailInvitationToApproveDefaultHeader()$
  <br/>
$elseif(view)$
  $_mailInvitationToViewDefaultHeader()$
  <br/>
$endif$

$if(hascustommessage)$
  <br/>

  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <p>$custommessage:br()$</p>
$endif$

  <br/>
  <strong>$_mailInvitationToSignFollowInstructions()$</strong>
  <br/>

### Reminding mails - SIGNED
###
###
remindMailSigned=
    $_remindMailSignedTitle()$
    $nicemail(
        mailheader= _remindMailSignedTitle();
        mailp1 = remindMailSignedStandardHeader()
    )$
###
remindMailSignedStandardHeader=
<br/>
  $remindMailSignedContractStandardHeaderSendRequest()$
  <br/>
  <br/>
  $if(mainfilelink)$
    $_mailDocumentLinked()$
  $elseif(shouldhavefilelink)$
    $_mailDocumentLinked()$
  $else$
    $_remindMailSignedContractStandardHeaderSendRequest2()$
  $endif$

$if(hascustommessage)$
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <p>$custommessage:br()$</p>
  <br/>
$else$
      <div class="editable" name="custommessage"></div>
$endif$
$if(mainfilelink)$
  $if(hascustommessage)$
    <br/>
  $endif$
  $nicebutton(link=mainfilelink; text=_mailDocumentLinkedButton())$
$elseif(shouldhavefilelink)$
  $if(hascustommessage)$
    <br/>
  $endif$
  $nicebutton(text=_mailDocumentLinkedButton())$
$endif$
###
remindMailSignedContractStandardHeaderSendRequest=
$if(sign)$
  $_remindMailSignedContractStandardHeaderSendRequest()$
$elseif(approve)$
  $_remindMailSignedContractStandardHeaderSendRequestApprove()$
$else$
  $_remindMailSignedContractStandardHeaderSendRequestReview()$
$endif$
###
### Reminding mails - NOT SIGNED
###
remindMailNotSignedContract=
    $remindMailNotSignedTitleContract()$

    $nicemail(
        mailheader=remindMailNotSignedTitleContract();
        mailp1=remindMailNotSignedContractBody();
        footer= legalparagraph()
    )$
    $remindMailNotSignedGoToActionMarkup()$
#####
remindMailNotSignedTitleContract=
$if(sign)$
  $_remindMailNotSignedTitleContract()$
$elseif(approve)$
  $_remindMailNotSignedTitleContractApprove()$
$else$
  $_remindMailNotSignedTitleContractReview()$
$endif$
###
remindMailNotSignedGoToActionMarkup=
$if(sign)$
  $gmailGoToActionMarkup(
      actionurl=link;
      actionname=_mailInvitationToSignGoToActionName();
      actiondescription=_mailInvitationToSignGoToActionDescription()
  )$
$elseif(approve)$
  $gmailGoToActionMarkup(
      actionurl=link;
      actionname=_mailInvitationToApproveGoToActionName();
      actiondescription=_mailInvitationToApproveGoToActionDescription()
  )$
$else$
  $gmailGoToActionMarkup(
      actionurl=link;
      actionname=_mailInvitationToViewGoToActionName();
      actiondescription=_mailInvitationToViewGoToActionDescription()
  )$
$endif$
#####
remindMailNotSignedContractBody=
$if(automatic)$
        $_remindMailNotSignedContractBodyAutomatic()$
$else$
        $if(sign)$
                $_remindMailNotSignedContractBody()$
        $elseif(approve)$
                $_remindMailNotApprovedContractBody()$
        $else$
                $_remindMailNotReviewedContractBody()$
        $endif$
$endif$
<BR/>
$if(custommessage)$
  <BR/>
  $_mailInvitationCustomInvitationHeader()$:
  <br/>
  <p>$custommessage:br()$</p>
  <br/>
$else$
  <BR/>
  <div class="editable" name="custommessage"></div>
$endif$
<strong>$_remindMailNotSignedContractBodyFurther()$</strong>
<BR/>
<BR/>
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
$if(signing)$
       $_invitationMailUndeliveredContentBody()$
$endif$
$if(viewing)$
       $_invitationMailUndeliveredContentBodyViewing()$
$endif$
$if(approving)$
       $_invitationMailUndeliveredContentBodyApproving()$
$endif$
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
###
### UNDELIVERED CONFIRMATION MAIL
###
confirmationMailUndelivered=
    $_confirmationMailUndeliveredTitle()$
    $nicemail(
        mailheader=_confirmationMailUndeliveredTitle();
        mailp1=confirmationMailUndeliveredMainP()
    )$
####
####
confirmationMailUndeliveredMainP =
       <br/>
       $_confirmationMailUndeliveredContentBody()$
       <br/>
       <br/>
       $nicebutton(link=concat(a1=ctxhostpart;a2=documentlink); text=_confirmationMailUndeliveredContentBodyButton())$
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
  $if(requestedby)$
    $_mailRequestChangeEmailByAdminExplanation()$
  $else$
    $_mailRequestChangeEmailExplanation()$
  $endif$
  <br/>
  <br/>
  $nicebutton(link=concat(a1=ctxhostpart;a2=link); text=_mailRequestChangeEmailButton())$
####
mailEmailChangeRequestedNotification=
  $_mailEmailChangeRequestedNotificationTitle()$
  $nicemail(
    mailheader=_mailEmailChangeRequestedNotificationTitle();
    mailp1=mailEmailChangeRequestedNotificationP()
  )$
####
mailEmailChangeRequestedNotificationP=
  $if(requestedbyadmin)$
    $_mailEmailChangeRequestedByAdminNotificationExplanation()$
  $else$
    $_mailEmailChangeRequestedNotificationExplanation()$
  $endif$
####
####   UTILS
####
legalparagraph=
$_legalparagraphBody()$
####
supportmaillink=
<a href='mailto:support@scrive.com'>support@scrive.com</a>
####
#### DOCUMENT INFORMATION PREVIEW FOR MAIL
####
####
concat=$a1:noescape()$$a2:noescape()$$a3:noescape()$
####
#### GMAIL ACTION MARKUP
####
gmailGoToActionMarkup=
<div itemscope="" itemtype="http://schema.org/EmailMessage">
<div itemprop="action" itemscope="" itemtype="http://schema.org/ViewAction">
<link itemprop="url" href="$actionurl$"></link>
<meta itemprop="name" content="$actionname$"></meta>
</div>
<meta itemprop="description" content="$actiondescription$"></meta>
</div>
####
