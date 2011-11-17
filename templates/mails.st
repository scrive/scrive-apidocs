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
        mailheader= _mailCancelDocumentByAuthorTitle();
        mailp1 = header;
        mailp2 = _mailCancelContractByAuthorContentBody();
        mailp3 = footer
    )$
#####
mailCancelOfferByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $nicemail(
        mailheader= _mailCancelDocumentByAuthorTitle();
        mailp1 = header;
        mailp2 = _mailCancelOfferByAuthorContentBody();
        mailp3 = footer
    )$
#####
mailCancelOrderByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $nicemail(
        mailheader= _mailCancelDocumentByAuthorTitle();
        mailp1 = header;
        mailp2 = _mailCancelOrderByAuthorContentBody();
        mailp3 = footer
    )$
#####
#####
##### MAIL AWAITING AUTHOR
#####
#####
mailDocumentAwaitingForAuthor=
    $_mailDocumentAwaitingForAuthorTitle()$
    $nicemail(
        mailheader= _mailDocumentAwaitingForAuthorTitle();
        mailp1 = mailDocumentAwaitingForAuthorContentMainP();
        mailp2 = footer
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
        mailheader = _mailRejectMailTitle();
        mailp1 = mailRejectMailContentHeader();
        mailp2 = _mailRejectContractMailContentContractRejected();
        mailp3 = footer
      )$
####
mailRejectOfferMail=
      $_mailRejectMailTitle()$
      $nicemail(
        mailheader = _mailRejectMailTitle();
        mailp1 = mailRejectMailContentHeader();
        mailp2 = _mailRejectOfferMailContentOfferRejected();
        mailp3 = footer
      )$
###
mailRejectOrderMail=
      $_mailRejectMailTitle()$
    $nicemail(
        mailheader = _mailRejectMailTitle();
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
        mailp2 = mailInvitationToSignContractIntructions();
        mailp3 = contractInfo();
        mailp4 = footer;
        mailp5 = legalparagraph()
    )$
###
mailInvitationToSignContractIntructions=
    $if(sign)$
    <p>$_mailInvitationToSignContractContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignContractContentIfNotSignatoryInstruction()$
    <br/>
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    <BR/>

####
mailInvitationToSignOffer=
     $mailInvitationTitle()$
     $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = mailInvitationToSignOfferIntructions();
        mailp3 = offerInfo();
        mailp4 = footer;
        mailp5 = legalparagraph()
    )$
###
mailInvitationToSignOfferIntructions=
    $if(sign)$
    <p>$_mailInvitationToSignOfferContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignOfferContentIfNotSignatoryInstruction()$
    <br/>
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$

###
mailInvitationToSignOrder=
    $mailInvitationTitle()$
    $nicemail(
        mailheader= mailInvitationTitle();
        mailp1 = header;
        mailp2 = mailInvitationToSignOrderIntructions();
        mailp3 = orderInfo();
        mailp4 = footer;
        mailp5 = legalparagraph()
    )$
####
mailInvitationToSignOrderIntructions=
    $if(sign)$
    <p>$_mailInvitationToSignOrderContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignOrderContentIfNotSignatoryInstruction()$
    <br/>
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
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
        mailp2=contractInfo();
        mailp3=_remindMailNotSignedContractContentInstruction();
        mailp4= legalparagraph()
    )$
#####
#####
remindMailNotSignedOffer=
    $_remindMailNotSignedTitle()$
    $nicemail(
        mailheader=_remindMailNotSignedTitle();
        mailp1=header;
        mailp2=_remindMailNotSignedOfferContentInstruction();
        mailp3=offerInfo();
        mailp4=footer;
        mailp5= legalparagraph()
    )$
###
remindMailNotSignedOrder=
    $_remindMailNotSignedTitle()$
    $nicemail(
        mailheader=_remindMailNotSignedTitle();
        mailp1=header;
        mailp2=_remindMailNotSignedOrderContentInstruction();
        mailp3=orderInfo();
        mailp4=footer;
        mailp5= legalparagraph()
    )$
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
<p>$_newUserMailContentBodyUserName()$ $email$</p>
<p>$_newUserMailContentBodySecond()$ <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a>
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

    <p>$_inviteCompanyAccountMailContentBodyUserName()$ $email$</p>

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
mailInviteUserAsCompanyAccount=
  $_mailInviteUserAsCompanyAccountTitle()$
  $nicemail(
      mailheader=_mailInviteUserAsCompanyAccountTitle();
      mailp1=mailInviteUserAsCompanyAccountP();
      mailp2=poweredBySkrivaPaPara()
    )$
####
mailInviteUserAsCompanyAccountP=
  <p>$_hi()$ $invited.fullnameOrEmail$,</p>
  <p>$_mailInviteUserAsCompanyAccountContentBodyFirst()$</p>

  <p>$_mailInviteUserAsCompanyAccountContentBodySecond()$</p>

  <p>$_mailInviteUserAsCompanyAccountContentInstructionFirst()$</p>
  <ul>$_mailInviteUserAsCompanyAccountContentInstructionSecond()$</ul>

  <p><a href="$hostpart$/account/bsa/$supervisor.id$">$_mailInviteUserAsCompanyAccountContentLink()$</a></p>
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
####
whohadsignedinfoforpreview=
    <span class="authornamewhennotsecretary" alt="$noonehadsignedtext()$"></span>
####
####
legalparagraph=
<p style="width:90%;font-size:8pt;font-style:italic;">$_legalparagraphBody()$ </p>
<BR/>
####
####
####
#### DOCUMENT INFORMATION PREVIEW FOR MAIL
####
contractInfo=
    $nicemailbodywithpreview(
        r1='true';
        r1cond='true';
        r1text= _remindMailNotSignedContractContentPartys();
        r1elems=partners;
        r1elemsclass = 'partnersmailpreview';


        r2='true';
        r2cond='true';
        r2text= _mailInvitationToSignContractContentSender();
        r2elems=creatorname;


        r3=nojavascriptmagic;
        r3cond=someonesigned;
        r3text= _whohadsignedcontractinfoformailIfSigned();
        r3elems=partnerswhosigned;
        r3alter='true';
        r3alttext = _whohadsignedcontractinfoformailIfNotSigned();
        r3altelems = noonehadsignedtext();


        r4=javascriptmagic;
        r4cond='true';
        r4text= _whohadsignedcontractinfoformailIfNotSigned();
        r4elems= whohadsignedinfoforpreview();

        r5='true';
        r5cond=timetosign;
        r5text= _timetosigninfo();
        r5elems=timetosign;

        r6='true';
        r6cond=isattachments;
        r6text= _appendixListAttachment();
        r6elems=attachments;

        r7='true';
        r7cond=hassigattachments;
        r7text=_sigattachlistRequestedAttachment();
        r7elems= sigattachments;
        r7separator= _from()
    )$

####
offerInfo=
    $nicemailbodywithpreview(
        r1='true';
        r1cond='true';
        r1text= _remindMailNotSignedOfferContentPartys();
        r1elems=partners;


        r2='true';
        r2cond='true';
        r2text= _remindMailNotSignedOfferContentSender();
        r2elems=creatorname;

        r3=nojavascriptmagic;
        r3cond=someonesigned;
        r3text= _whohadsignedofferinfoformailIfConfirmed();
        r3elems=partnerswhosigned;
        r3alter='true';
        r3alttext = _whohadsignedofferinfoformailIfNotConfirmed();
        r3altelems = noonehadsignedtext();


        r4=javascriptmagic;
        r4cond='true';
        r4text= _whohadsignedofferinfoformailIfNotConfirmed();
        r4elems= whohadsignedinfoforpreview();

        r5='true';
        r5cond=timetosign;
        r5text= _timetosigninfo();
        r5elems=timetosign;

        r6='true';
        r6cond=isattachments;
        r6text= _appendixListAttachment();
        r6elems=attachments;

        r7='true';
        r7cond=hassigattachments;
        r7text=_sigattachlistRequestedAttachment();
        r7elems= sigattachments;
        r7separator= _from()
    )$
####
orderInfo=
    $nicemailbodywithpreview(
        r1='true';
        r1cond='true';
        r1text= _remindMailNotSignedOrderContentPartys();
        r1elems=partners;

        r2='true';
        r2cond='true';
        r2text= _mailInvitationToSignOrderContentSender();
        r2elems=creatorname;

        r3=nojavascriptmagic;
        r3cond=someonesigned;
        r3text= _whohadsignedorderinfoformailIfAccepted();
        r3elems=partnerswhosigned;
        r3alter='true';
        r3alttext = _whohadsignedorderinfoformailIfNotAccepted();
        r3altelems = noonehadsignedtext();

        r4=javascriptmagic;
        r4cond='true';
        r4text= _whohadsignedorderinfoformailIfNotAccepted();
        r4elems= whohadsignedinfoforpreview();


        r5='true';
        r5cond=timetosign;
        r5text= _timetosigninfo();
        r5elems=timetosign;

        r6='true';
        r6cond=isattachments;
        r6text= _appendixListAttachment();
        r6elems=attachments;

        r7='true';
        r7cond=hassigattachments;
        r7text=_sigattachlistRequestedAttachment();
        r7elems= sigattachments;
        r7separator= _from()
    )$

####
####
nicemailbodywithpreview=
<table align="center" style="margin: auto; border-collapse: separate;  border-spacing: 30px;">
    <tr>
       <td>
         <table cellspacing="0" cellpadding="0" style="margin: auto;">
           <tr>
               <td> <div style="width:190px;height:270px;border:1px solid #CCCCCC"><img src="$ctxhostpart$$previewLink$" height="270" width="190" alt=""/></div></td>
           </tr>
         </table>
       </td>
       <td>

        <table style="font-family:'Lucida Grande', sans-serif;font-size:12px;margin: auto;">
            <tr>
                <td><span style="color:#212121;font-size:1.5em;">$documenttitle$</span></td>
            </tr>
               $if(r1)$
                $nicemailpreviewrow(
                    cond=r1cond;
                    text= r1text;
                    elems=r1elems;
                    alter=r1alter;
                    alttext = r1alttext;
                    altelems = r1altelems;
                    elemsclass = r1elemsclass;
                    separator = r1separator
                )$
              $endif$


            $if(r2)$
                $nicemailpreviewrow(
                    cond=r2cond;
                    text= r2text;
                    elems=r2elems;
                    alter=r2alter;
                    alttext = r2alttext;
                    altelems = r2altelems;
                    elemsclass = r2elemsclass;
                    separator = r2separator
                )$
            $endif$

            $if(r3)$
                $nicemailpreviewrow(
                    cond=r3cond;
                    text= r3text;
                    elems=r3elems;
                    alter=r3alter;
                    alttext = r3alttext;
                    altelems = r3altelems;
                    elemsclass = r3elemsclass;
                    separator = r3separator
                )$
            $endif$

            $if(r4)$
                $nicemailpreviewrow(
                    cond=r4cond;
                    text= r4text;
                    elems=r4elems;
                    alter=r4alter;
                    alttext = r4alttext;
                    altelems = r4altelems;
                    elemsclass = r4elemsclass;
                    separator = r4separator
                )$
            $endif$

            $if(r5)$
                $nicemailpreviewrow(
                    cond=r5cond;
                    text= r5text;
                    elems=r5elems;
                    alter=r5alter;
                    alttext = r5alttext;
                    altelems = r5altelems;
                    elemsclass = r5elemsclass;
                    separator = r5separator
                )$
            $endif$

            $if(r6)$
                $nicemailpreviewrow(
                    cond=r6cond;
                    text= r6text;
                    elems=r6elems;
                    alter=r6alter;
                    alttext = r6alttext;
                    altelems = r6altelems;
                    elemsclass = r6elemsclass;
                    separator = r6separator
                )$
            $endif$

            $if(r7)$
                $nicemailpreviewrow(
                    cond=r7cond;
                    text= r7text;
                    elems=r7elems;
                    alter=r7alter;
                    alttext = r7alttext;
                    altelems = r7altelems;
                    elemsclass = r7elemsclass;
                    separator = r7separator
                )$
            $endif$
        </table>
       </td>
   </tr>
</table>
####
nicemailpreviewrow=
$if(cond)$
<tr><td>
      <p style="font-style:italic;font-family:sans-serif;vertical-align:top;font-size:11px;">$text$</p>
          $elems: {elem |
              $if(elem.0)$
                <p style="font-weight:bold;"> $elem.0$ $separator$ $elem.1$</p>
              $else$
                <p style="font-weight:bold;" $if(elemsclass)$class="$elemsclass$"$endif$> $elem$ </p>
              $endif$
          }$
</td></tr>
$elseif(alter)$
$nicemailpreviewrow(
        cond='true';
        text=alttext;
        elems=altelems
    )$
$endif$
####
mailMailAPIError=
    Scrive by Mail Error
    $nicemail(
        mailheader= "ScriveBy Mail Error";
        mailp1 = "<p>Greetings from Scrive by Mail!</p>";
        mailp2 = "<p>I'm sorry, but I was unable to process the mail you just sent me and I have not forwarded the document to anyone.</p>";
        mailp3 = errormsg;
        mailp4 = "<p>Thanks for using Scrive by Mail</p>"
    )$
#####
