#####
##### MAILS - Separated in sections
#####
#####
##### CANCEL MAIL
#####
####
mailCancelContractByAuthorStandardHeader=
    <p>$_hi()$,</p>
######
mailCancelOfferByAuthorStandardHeader=
    <p>$_hi()$,</p>
######
mailCancelOrderByAuthorStandardHeader=
    <p>$_hi()$,</p>
######
mailCancelContractByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = _mailCancelContractByAuthorContentBody();
        mailp3 = footer
    )$
#####
mailCancelOfferByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = _mailCancelOfferByAuthorContentBody();
        mailp3 = footer
    )$
#####
mailCancelOrderByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = _mailCancelOrderByAuthorContentBody();
        mailp3 = footer
    )$
#####
#####
##### MAIL AWAITING AUTHOR
#####
#####
mailDocumentAwaitingForAuthorContent=
    $_mailDocumentAwaitingForAuthorTitle()$
    $nicemail(
        mailheader= _mailDocumentAwaitingForAuthorTitle();
        mailp1 = mailDocumentAwaitingForAuthorContentMainP();
        mailp3 = footer
    )$
#####
mailDocumentAwaitingForAuthorContentMainP = 
   <p>$_hi()$,</p>
   <p>$_mailDocumentAwaitingForAuthorContentSignedBy()$</p>
   <p>$_mailDocumentAwaitingForAuthorContentInstruction()$</p>
#####
##### CLOSED MAIL
#####
mailContractClosed=
    $_mailDocumentClosedTitle()$
    $nicemail(
        mailheader = _mailDocumentClosedTitle();
        mailp1 = mailContractClosedMainP();
        mailp2 = footer
    )$
    
####
mailContractClosedMainP = 
      <p>$_hi()$,</p>
      <p>$_mailContractClosedContentSignedBy()$</p>
      <p>$_mailContractClosedContentAttached()$</p> 
    
####
mailOfferClosed=
    $_mailDocumentClosedTitle()$
    $nicemail(
        mailheader = _mailDocumentClosedTitle();
        mailp1 = mailOfferClosedMainP();
        mailp2 = footer
    )$
    $footer:noescape()$
####
mailOfferClosedMainP = 
    <p>$_hi()$,</p>
      <p>
       $if(!service)$
        $_mailOfferClosedContentOfferAccepted()$
       $else$
        $_mailOfferClosedContentOfferAcceptedWithService()$
       $endif$
       </p>
      <p>$_mailOfferClosedContentAttached()$</p>
####
mailOrderClosed=
    $_mailDocumentClosedTitle()$
    $nicemail(
        mailheader = _mailDocumentClosedTitle();
        mailp1 = mailOrderClosedMainP();
        mailp2 = footer
    )$
####
mailOrderClosedMainP=
      <p>$_hi()$,</p>
      <p>
       $if(!service)$
        $_mailOrderClosedContentViewed()$
       $else$
        $_mailOrderClosedContentViewedWithService()$
       $endif$
      </p>
      <p>$_mailOrderClosedContentAttached()$</p>
####
#### REJECTED MAIL
####
mailRejectContractMail=
      $_mailRejectMailTitle()$
      $nicemail(
        mailheader = _mailDocumentClosedTitle();
        mailp1 = mailRejectMailContentHeader();
        mailp2 = _mailRejectContractMailContentContractRejected();
        mailp3 = footer
      )$
####
mailRejectOfferMail=
      $_mailRejectMailTitle()$
      $nicemail(
        mailheader = _mailDocumentClosedTitle();
        mailp1 = mailRejectMailContentHeader();
        mailp2 = _mailRejectOfferMailContentOfferRejected();
        mailp3 = footer
      )$
###
mailRejectOrderMail=
      $_mailRejectMailTitle()$
    $nicemail(
        mailheader = _mailDocumentClosedTitle();
        mailp1 = mailRejectMailContentHeader();
        mailp2 = _mailRejectOrderMailContentHasRejected();
        mailp3 = footer
      )$
###
mailRejectMailContentHeader=
      $if(customMessage)$
       $customMessage:noescape()$ 
      $else$
      <div class="editable" name="customtext">
      <p>$_hi()$,</p>
      </div>
      $endif$
####
#### INVITATION MAILS
####
mailInvitationTitle=
    $if(sign)$
        $_mailInvitationToSignTitle()$
    $elseif(send)$
        $_mailInvitationToSignTitle()$
    $elseif(view)$
        $_mailInvitationToViewTitle()$
    $endif$
###
mailInvitationToSignContract=
    $mailInvitationTitle()$
     $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = mailInvitationToSignContractMainP();
        mailp3 = mailInvitationToSignContractIntructions();
        mailp4 = legalparagraph()
    )$
###
mailInvitationToSignContractMainP=
<p>
     <i>$_mailInvitationToSignContractContentInfo()$</i><br/>
     $_mailInvitationToSignContractContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedinfo:noescape()$<br/>
     $_mailInvitationToSignContractContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
     $appendixList()$
     $sigattachlist()$
</p>
###
mailInvitationToSignContractIntructions=
    $if(issignatory)$
    <p>$_mailInvitationToSignContractContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignContractContentIfNotSignatoryInstruction()$
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    <BR/>
    $footer:noescape()$ 

####
mailInvitationToSignOffer=
     $mailInvitationTitle()$
     $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = mailInvitationToSignOfferMainP();
        mailp3 = mailInvitationToSignOfferIntructions();
        mailp4 = legalparagraph()
    )$
###
mailInvitationToSignOfferMainP=
    <p>
     <i>$_mailInvitationToSignOfferContentInfo()$</i><br/>
     $_mailInvitationToSignOfferContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedinfo:noescape()$<br/>
     $_mailInvitationToSignOfferContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
     $appendixList()$
     $sigattachlist()$
    </p>
###
mailInvitationToSignOfferIntructions=
    $if(issignatory)$
    <p>$_mailInvitationToSignOfferContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignOfferContentIfNotSignatoryInstruction()$
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    $footer:noescape()$
###
mailInvitationToSignOrder=
    $mailInvitationTitle()$
    $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = mailInvitationToSignOrderMainP();
        mailp3 = mailInvitationToSignOrderIntructions();
        mailp4 = legalparagraph()
    )$
####
mailInvitationToSignOrderMainP=
    <p>
     <i>$_mailInvitationToSignOrderContentInfo()$</i><br/>
     $_mailInvitationToSignOrderContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedinfo:noescape()$<br/>
     $_mailInvitationToSignOrderContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
     $appendixList()$
     $sigattachlist()$
    </p>
####
mailInvitationToSignOrderIntructions=
    $if(issignatory)$
    <p>$_mailInvitationToSignOrderContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignOrderContentIfNotSignatoryInstruction()$
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    $footer:noescape()$
####
mailInvitationToSignContractDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      $if(!service)$
        $_mailInvitationToSignContractDefaultHeader()$ 
      $else$
        $_mailInvitationToSignContractDefaultHeaderWithService()$
      $endif$
   </p>
####
mailInvitationToSignOfferDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      $_mailInvitationToSignOfferDefaultHeader()$ 
    </p>
####
mailInvitationToSignOrderDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      $_mailInvitationToSignOrderDefaultHeader()$ 
    </p>
####
mailInvitationToViewDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      $if(!service)$
       $_mailInvitationToViewDefaultHeader()$
      $else$
       $_mailInvitationToViewDefaultHeaderWithService()$  
      $endif$
     </p>
###
###
### Reminding mails - SIGNED
###
###
remindMailSigned=
    $documenttitle$
    $nicemail(
        mailheader= documenttitle;
        mailp1 = header;
        mailp2 = footer
    )$
###
remindMailSignedContractStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>
       $if(!service)$
        $_remindMailSignedContractStandardHeaderSendRequest()$
       $else$ 
        $_remindMailSignedContractStandardHeaderSendRequestWithService()$
       $endif$
      </p>
    </span>
####
remindMailSignedOfferStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>
       $if(!service)$
        $_remindMailSignedOfferStandardHeaderSendRequest()$
       $else$ 
        $_remindMailSignedOfferStandardHeaderSendRequestWithService()$
       $endif$
     </p>
    </span>
####
remindMailSignedOrderStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>
        $if(!service)$
         $_remindMailSignedOrderStandardHeaderSendRequest()$
        $else$ 
         $_remindMailSignedOrderStandardHeaderSendRequestWithService()$
       $endif$
      </p>
    </span>
###
###
### Reminding mails - NOT SIGNED
###
###
remindMailNotSignedContractStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>$_remindMailNotSignedContractStandardHeaderRemind()$</p>
    </span>
#####
remindMailNotSignedOfferStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>$_remindMailNotSignedOfferStandardHeaderRemind()$</p>
    </span>
#####
remindMailNotSignedOrderStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>$_remindMailNotSignedOrderStandardHeaderRemind()$</p>
    </span>
#####
remindMailNotSignedContract=
    $_remindMailNotSignedTitle()$

    $nicemail(
        mailheader=_remindMailNotSignedTitle();
        mailp1=header;
        mailp2=remindMailNotSignedContractMainP();
        mailp3=_remindMailNotSignedContractContentInstruction();
        mailp4=footer;
        mailp5= legalparagraph()
    )$
#####
remindMailNotSignedContractMainP=
<p>
    <i>$_remindMailNotSignedContractContentInfo()$</i><br/>
    $_remindMailNotSignedContractContentPartys()$ $partnersinfo:noescape()$<br/>
    $whohadsignedinfo:noescape()$<br/>
    $timetosigninfo()$
    $appendixList()$
</p>
#####
remindMailNotSignedOffer=
    $_remindMailNotSignedTitle()$
    $nicemail(
        mailheader=_remindMailNotSignedTitle();
        mailp1=header;
        mailp2=remindMailNotSignedOfferMainP();
        mailp3=_remindMailNotSignedOfferContentInstruction();
        mailp4=footer;
        mailp5= legalparagraph()
    )$
#####
remindMailNotSignedOfferMainP=
    <span>
      <p>
        <i>$_remindMailNotSignedOfferContentInfo()$</i><br/>
        $_remindMailNotSignedOfferContentPartys()$ $partnersinfo:noescape()$<br/>
        $whohadsignedinfo:noescape()$<br/>
        $_remindMailNotSignedOfferContentSender()$ <strong>$creatorname$</strong><br/>
        $timetosigninfo:noescape()$
        $appendixList()$
        $sigattachlist()$
      </p>
      <p>$_remindMailNotSignedOfferContentInstruction()$</p>
    </span>
###
remindMailNotSignedOrder=
    $_remindMailNotSignedTitle()$
    $nicemail(
        mailheader=_remindMailNotSignedTitle();
        mailp1=header;
        mailp2=remindMailNotSignedOrderMainP();
        mailp3=_remindMailNotSignedOrderContentInstruction();
        mailp4=footer;
        mailp5= legalparagraph()
    )$
###
remindMailNotSignedOrderMainP=
    <span>
      <p><i>$_remindMailNotSignedOrderContentInfo()$</i><br/>
        $_remindMailNotSignedOrderContentPartys()$ $partnersinfo:noescape()$<br/>
        $whohadsignedinfo:noescape()$<br/> 
        $timetosigninfo:noescape()$
        $appendixList()$
      </p>
      <p>$_remindMailNotSignedOrderContentInstruction()$</p>
    </span>
###
###
###  NEW USER MAIN
### 
###
newUserMail=
$_newUserMailTitle()$
$nicemail(
        mailheader=_newUserMailTitle();
        mailp1=newUserMailMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
####
newUserMailMainP=
<p>$_hi()$,</p>
<p>$_newUserMailContentBodyFirst()$</p>
<p>$_newUserMailContentBodySecond()$</p>
<p>$_newUserMailContentBodyUserName()$ $email$</p>
<p>$_newUserMailContentBodyThird()$ <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a>
</p>
####
#### INVITE TO COMPANY MAIL
####
inviteCompanyAccountMail=
    $_inviteCompanyAccountMailTitle()$
    $nicemail(
        mailheader=_inviteCompanyAccountMailTitle();
        mailp1=inviteCompanyAccountMailMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
####
inviteCompanyAccountMailMainP=
 <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_inviteCompanyAccountMailContentBodyFirst()$</p>

    <p>$_inviteCompanyAccountMailContentBodyUserName()$<br/>
    $email$</p>
    
    <p>$_inviteCompanyAccountMailContentClickLink()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

####
#### CREATING ACCOUNTS AFTER SIGNING
####
mailAccountBySigningContractReminder=
$_mailAccountBySigningContractReminderTitle()$
$nicemail(
        mailheader=_mailAccountBySigningContractReminderTitle();
        mailp1=mailAccountBySigningContractReminderMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
mailAccountBySigningContractReminderMainP=
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_mailAccountBySigningContractReminderContentBody()$</p>

    <p>$_mailAccountBySigningContractReminderContentInstruction()$</p>
###
mailAccountBySigningOfferReminder=
$_mailAccountBySigningOfferReminderTitle()$
$nicemail(
        mailheader=_mailAccountBySigningOfferReminderTitle();
        mailp1=mailAccountBySigningOfferReminderMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
mailAccountBySigningOfferReminderMainP = 
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_mailAccountBySigningOfferReminderContentBody()$</p>

    <p>$_mailAccountBySigningOfferReminderContentInstruction()$</p>
###
mailAccountBySigningOrderReminder=
$_mailAccountBySigningOrderReminderTitle()$
$nicemail(
        mailheader=_mailAccountBySigningOrderReminderMainP();
        mailp1=mailAccountBySigningOrderReminderMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
mailAccountBySigningOrderReminderMainP=
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_mailAccountBySigningOrderReminderContentBody()$</p>

    <p>$_mailAccountBySigningOrderReminderContentInstruction()$</p>
###
###
### VIRAL INVITE
###
###
mailViralInvite=
    $_mailViralInviteTitle()$
    $nicemail(
        mailheader=_mailViralInviteTitle();
        mailp1=mailViralInviteMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
####
mailViralInviteMainP = 
<p>$_hi()$,</p>

    <p>$_mailViralInviteContentBodyFirst()$</p>

    <p>$_mailViralInviteContentBodySecond()$</p>

    <p>$_mailViralInviteContentBodyUserName()$ $email$</p>
    
    <p>$_mailViralInviteContentBodyThird()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

###
###
### NEW ACCOUNT BY ADMIN
###
mailNewAccountCreatedByAdminContentHeader=
$if(custommessage)$
    $custommessage:noescape()$
    $else$
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_mailNewAccountCreatedByAdminContentHeaderBody()$</p>
$endif$
###
mailNewAccountCreatedByAdmin=
   $_mailNewAccountCreatedByAdminTitle()$
    $nicemail(
        mailheader=_mailNewAccountCreatedByAdminTitle();
        mailp1=mailNewAccountCreatedByAdminContentHeader();
        mailp2=mailNewAccountCreatedByAdminMainP();
        mailp3=poweredBySkrivaPaPara()
    )$
####
mailNewAccountCreatedByAdminMainP=
    <p>$_mailNewAccountCreatedByAdminContentUserName()$ $email$</p>
    
    <p>$_mailNewAccountCreatedByAdminContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>
####
####
invitationMailDeferred=
    $_invitationMailDeferredTitle()$
    $nicemail(
        mailheader=_invitationMailDeferredTitle();
        mailp1=invitationMailDeferredMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
invitationMailDeferredMainP=
      <p>$_hi()$ $authorname$,</p>
      <p>$_invitationMailDeferredContentBodyFirst()$</p>
      <p>$_invitationMailDeferredContentBodySecond()$ <a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a></p>
      
###
###
invitationMailDeliveredAfterDeferred=
    $_invitationMailDeliveredAfterDeferredTitle()$
    $nicemail(
        mailheader=_invitationMailDeliveredAfterDeferredTitle();
        mailp1=invitationMailDeliveredAfterDeferredMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
invitationMailDeliveredAfterDeferredMainP = 
      <p>$_hi()$ $authorname$,</p>
      <p>$_invitationMailDeliveredAfterDeferredContentBody()$</p>
###
### UNDELIVERED MAIL
###
invitationMailUndelivered=
    $_invitationMailUndeliveredTitle()$
    $nicemail(
        mailheader=_invitationMailUndeliveredTitle();
        mailp1=invitationMailUndeliveredMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
####
####
invitationMailUndeliveredMainP = 
       <p>$_hi()$ $authorname$,</p>
       <p>$_invitationMailUndeliveredContentBody()$</p>
       <p>$_invitationMailUndeliveredContentInstruction()$</p>
####
#### DOCUMENT ERROR
####
mailDocumentError=
    $_mailDocumentErrorTitle()$
    $nicemail(
        mailheader=_mailDocumentErrorTitle();
        mailp1=mailDocumentErrorMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
mailDocumentErrorMainP = 
    <p>$_hi()$,</p>
    <p>$_mailDocumentErrorContentBodyFirst()$</p>
    <p>$_mailDocumentErrorContentBodySecond()$</p>
    <p>$_mailDocumentErrorContentBodyThird()$</p>
    <p>$_mailDocumentErrorContentBodyFourth()$</p>
####
#### CHANGE PASSWORD MAIL
####
passwordChangeLinkMail=
    $_passwordChangeLinkMailTitle()$
    $nicemail(
        mailheader=_passwordChangeLinkMailTitle();
        mailp1=passwordChangeLinkMailMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
passwordChangeLinkMailMainP=
    <p>$_hi()$ $personname$,</p>
    <p>$_passwordChangeLinkMailContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

###
### MISMATCH MAILS
###
mailMismatchSignatory=
$_mailMismatchSignatoryTitle()$
    $nicemail(
        mailheader=_mailMismatchSignatoryTitle();
        mailp1=mailMismatchSignatoryMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
#####
mailMismatchSignatoryMainP=
<p>$_hi()$ $signame$,</p>

<p>$_mailMismatchSignatoryContentBodyFirst()$</p>

<p>$_mailMismatchSignatoryContentBodySecond()$</p>

$if(messages)$
<p>$_mailMismatchSignatoryContentBodyIfMessages()$</p>
$messages:noescape()$
$endif$

<p>$_mailMismatchSignatoryContentBestRegards()$</p>
####
mailMismatchAuthor=
$_mailMismatchAuthorTitle()$
$nicemail(
        mailheader=_mailMismatchAuthorTitle();
        mailp1=mailMismatchAuthorMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
####
mailMismatchAuthorMainP = 
<p>$_hi()$ $authorname$,</p>
<p>$_mailMismatchAuthorContentBodyFirst()$</p>
<p>$_mailMismatchAuthorContentBodySecond()$</p>
<p>$_mailMismatchAuthorContentErrorReport()$</p>
$messages:noescape()$
<p>$_mailMismatchAuthorContentBestRegards()$</p>
#### 
####
mailInviteUserAsCompanyAccountTitle=$_mailInviteUserAsCompanyAccountTitle()$
###
mailInviteUserAsCompanyAccountContent=
  <p>$_hi()$ $invited.fullnameOrEmail$,</p>
  <p>$_mailInviteUserAsCompanyAccountContentBodyFirst()$</p>

 <p>$_mailInviteUserAsCompanyAccountContentBodySecond()$</p>

 <p>$_mailInviteUserAsCompanyAccountContentInstructionFirst()$</p>
 <ul>$_mailInviteUserAsCompanyAccountContentInstructionSecond()$
</ul>

 <p><a href="$hostpart$/account/bsa/$supervisor.id$">$_mailInviteUserAsCompanyAccountContentLink()$</a></p>
  $poweredBySkrivaPaPara()$
####
####
####
mailCompanyAccountAccepted=
$_mailCompanyAccountAcceptedTitle()$
$nicemail(
        mailheader=_mailCompanyAccountAcceptedTitle();
        mailp1=mailCompanyAccountAcceptedMainP();
        mailp2=poweredBySkrivaPaPara()
    )$
###
mailCompanyAccountAcceptedMainP=
  <p>$_hi()$ $user.fullnameOrEmail$,</p>
  <p>$_mailCompanyAccountAcceptedContentBody()$</p>
####
#### Mail API EMAILS
####
mailMailAPIConfirmContract= 
   $_mailMailAPIConfirmTitle()$
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=mailMailAPIConfirmContractMainP();
        mailp2=footer
    )$
####
mailMailAPIConfirmContractMainP= 
$_mailMailAPIConfirmContractHeader()$
    <p>
     <i>$_mailInvitationToSignContractContentInfo()$</i><br/>
     $_mailInvitationToSignContractContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignContractContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
    </p>
####
mailMailAPIConfirmOrder=
   $_mailMailAPIConfirmTitle()$ 
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=mailMailAPIConfirmOrderMainP();
        mailp2=footer
    )$
####
mailMailAPIConfirmOrderMainP=
$_mailMailAPIConfirmOrderHeader()$
   <p>
     <i>$_mailInvitationToSignOrderContentInfo()$</i><br/>
     $_mailInvitationToSignOrderContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignOrderContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
    </p>
####
mailMailAPIConfirmOffer= 
   $_mailMailAPIConfirmTitle()$
   $_mailMailAPIConfirmTitle()$ 
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=mailMailAPIConfirmOfferMainP();
        mailp2=footer
    )$
####
mailMailAPIConfirmOfferMainP= 
    $_mailMailAPIConfirmOfferHeader()$
    <p>
     <i>$_mailInvitationToSignOfferContentInfo()$</i><br/>
     $_mailInvitationToSignOfferContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignOfferContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
    </p>
####
####
####   UTILS
####
####
timetosigninfo=
    <span>$_timetosigninfo()$ <strong>$time$</strong>.</span><br/>
###
updateinglistwithauthor=
    <span>
      <span class="partylistupdate"></span>
    </span>
####
noonehadsignedtext=$_noonehadsignedtext()$
####
contracthassignedformailtext=$_contracthassignedformailtext()$
####
offerhassignedformailtext=$_offerhassignedformailtext()$
####
orderhassignedformailtext=$_orderhassignedformailtext()$
####
whohadsignedcontractinfoformail=
    $if(signedlist)$    $_whohadsignedcontractinfoformailIfSigned()$ $signedlist:noescape()$
    $else$  $_whohadsignedcontractinfoformailIfNotSigned()$ $noonehadsignedtext()$
    $endif$
####
whohadsignedorderinfoformail=
    $if(signedlist)$    $_whohadsignedorderinfoformailIfAccepted()$ $signedlist:noescape()$
    $else$  $_whohadsignedorderinfoformailIfNotAccepted()$ $noonehadsignedtext()$
    $endif$
####
whohadsignedofferinfoformail=
    $if(signedlist)$    $_whohadsignedofferinfoformailIfConfirmed()$$signedlist:noescape()$
    $else$  $_whohadsignedofferinfoformailIfNotConfirmed()$ $noonehadsignedtext()$
    $endif$
####
whohadsignedinfoforpreview=
    <span class="authornamewhennotsecretary" alt="$noonehadsignedtext()$" okprefix="$_whohadsignedinfoforpreview()$"></span>
####
####
legalparagraph=
<p style="width:90%;font-size:8pt;font-style:italic;">$_legalparagraphBody()$ </p>
<BR/>
####
appendixList=$if(isattachments)$ $_appendixListAttachment()$ $attachments:{a | $a$ };separator=", "$ <br />$endif$
###
sigattachlist=$if(hassigattachments)$ $_sigattachlistRequestedAttachment()$ $sigattachments:{sa|$sa.name:strong()$ $_from()$ $sa.sigs:noescape()$};separator="; "$ <br />$endif$
####