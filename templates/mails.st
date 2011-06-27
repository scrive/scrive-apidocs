######
mailCancelDocumentByAuthorTitle=$_mailCancelDocumentByAuthorTitle()$
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
mailCancelContractByAuthorContent=
    $header:noescape()$
    <p>$_mailCancelContractByAuthorContentBody()$</p>
    $footer:noescape()$
#####
mailCancelOfferByAuthorContent=
    $header:noescape()$
    <p>$_mailCancelOfferByAuthorContentBody()$</p>
    $footer:noescape()$
#####
mailCancelOrderByAuthorContent=
    $header:noescape()$
    <p>$_mailCancelOrderByAuthorContentBody()$</p>
    $footer:noescape()$
#####
mailDocumentAwaitingForAuthorTitle=$_mailDocumentAwaitingForAuthorTitle()$
######
mailDocumentAwaitingForAuthorContent=
    <p>$_hi()$,</p>
    <p>$_mailDocumentAwaitingForAuthorContentSignedBy()$</p>
    <p>$_mailDocumentAwaitingForAuthorContentInstruction()$</p>
    $footer:noescape()$
#####
mailDocumentClosedTitle=$_mailDocumentClosedTitle()$
#####
mailContractClosedContent=
    <span>
      <p>$_hi()$,</p>
      <p>$_mailContractClosedContentSignedBy()$</p>
      <p>$_mailContractClosedContentAttached()$</p> 
      $footer:noescape()$
    </span>
####
mailOfferClosedContent=
    <span>
      <p>$_hi()$,</p>
      <p>$_mailOfferClosedContentOfferAccepted()$</p>
      <p>$_mailOfferClosedContentAttached()$</p>
      $footer:noescape()$
    </span>
####
mailOrderClosedContent=
    <span>
      <p>$_hi()$,</p>
      <p>$_mailOrderClosedContentViewed()$</p>
      <p>$_mailOrderClosedContentAttached()$</p>
      $footer:noescape()$
    </span>
####
mailRejectMailTitle=$_mailRejectMailTitle()$
####
mailRejectContractMailContent=
      $mailRejectMailContentHeader()$
      <p>$_mailRejectContractMailContentContractRejected()$</p>
      $footer:noescape()$
####
mailRejectOfferMailContent=
      $mailRejectMailContentHeader()$
      <p>$_mailRejectOfferMailContentOfferRejected()$</p>
      $footer:noescape()$
###
mailRejectOrderMailContent=
      $mailRejectMailContentHeader()$
      <p>$_mailRejectOrderMailContentHasRejected()$
      </p>
      $footer:noescape()$
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
mailInvitationToViewTitle=$_mailInvitationToViewTitle()$
###
mailInvitationToSignTitle=$_mailInvitationToSignTitle()$
###
mailInvitationToSignContractContent=
    $header:noescape()$
    <p>
     <i>$_mailInvitationToSignContractContentInfo()$</i><br/>
     $_mailInvitationToSignContractContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedcontractinfo:noescape()$<br/>
     $_mailInvitationToSignContractContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
     $appendixList()$
     $sigattachlist()$
    </p>
    $if(issignatory)$
    <p>$_mailInvitationToSignContractContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignContractContentIfNotSignatoryInstruction()$
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    $footer:noescape()$
    $legalparagraph()$
###
mailInvitationToSignOfferContent=
    $header:noescape()$
    <p>
     <i>$_mailInvitationToSignOfferContentInfo()$</i><br/>
     $_mailInvitationToSignOfferContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedofferinfo:noescape()$<br/>
     $_mailInvitationToSignOfferContentSender()$<strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
     $appendixList()$
     $sigattachlist()$
    </p>
    $if(issignatory)$
    <p>$_mailInvitationToSignOfferContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignOfferContentIfNotSignatoryInstruction()$
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    $footer:noescape()$
    $legalparagraph()$
###
mailInvitationToSignOrderContent=
    $header:noescape()$
    <p>
     <i>$_mailInvitationToSignOrderContentInfo()$</i><br/>
     $_mailInvitationToSignOrderContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedorderinfo:noescape()$<br/>
     $_mailInvitationToSignOrderContentSender()$<strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
     $appendixList()$
     $sigattachlist()$
    </p>
    $if(issignatory)$
    <p>$_mailInvitationToSignOrderContentIfSignatoryInstruction()$</p>
    $else$
    $_mailInvitationToSignOrderContentIfNotSignatoryInstruction()$
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    $footer:noescape()$
    $legalparagraph()$
###
mailInvitationToSignContractDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      <strong>$creatorname$</strong>$_mailInvitationToSignContractDefaultHeaderBody1()$<strong>$documenttitle$</strong>$_mailInvitationToSignContractDefaultHeaderBody2()$</p>
####
mailInvitationToSignOfferDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      <strong>$creatorname$</strong>$_mailInvitationToSignOfferDefaultHeaderBody1()$<strong>$documenttitle$</strong>$_mailInvitationToSignOfferDefaultHeaderBody2()$</p>
####
mailInvitationToSignOrderDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      <strong>$creatorname$</strong>$_mailInvitationToSignOrderDefaultHeaderBody1()$<strong>$documenttitle$</strong>$_mailInvitationToSignOrderDefaultHeaderBody2()$</p>
####
mailInvitationToViewDefaultHeader=
    <p>$_hi()$ $personname$,
      <br/><br/>
      <strong>$creatorname$</strong> $_mailInvitationToViewDefaultHeaderBody1()$<strong>$documenttitle$</strong>$_mailInvitationToViewDefaultHeaderBody2()$</p>
####
remindMailSignedTitle=$documenttitle$
###
remindMailSignedContractStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>$_remindMailSignedContractStandardHeaderSendRequest()$</p>
    </span>
####
remindMailSignedOfferStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>$_remindMailSignedOfferStandardHeaderSendRequest()$</p>
    </span>
####
remindMailSignedOrderStandardHeader=
    <span>
      <p>$_hi()$ <strong>$personname$</strong></p>
      <p>$_remindMailSignedOrderStandardHeaderSendRequest()$</p>
    </span>
####
remindMailNotSignedTitle=$_remindMailNotSignedTitle()$
########
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
remindMailNotSignedContractContent=
    $header:noescape()$
    <span>
      <p><i>$_remindMailNotSignedContractContentInfo()$</i><br/>
        $_remindMailNotSignedContractContentPartys()$ $partnersinfo:noescape()$<br/>
        $whohadsignedinfo:noescape()$<br/> 
        $timetosigninfo:noescape()$
        $appendixList()$
      </p>
      <p>$_remindMailNotSignedContractContentInstruction()$</p>
    </span>
    $footer:noescape()$
    $legalparagraph()$
#####
remindMailNotSignedOfferContent=
    $header:noescape()$
    <span>
      <p>
        <i>$_remindMailNotSignedOfferContentInfo()$</i><br/>
        $_remindMailNotSignedOfferContentPartys()$ $partnersinfo:noescape()$<br/>
        $whohadsignedofferinfo:noescape()$<br/>
        $_remindMailNotSignedOfferContentSender()$<strong>$creatorname$</strong><br/>
        $timetosigninfo:noescape()$
        $appendixList()$
        $sigattachlist()$
      </p>
      <p>$_remindMailNotSignedOfferContentInstruction()$</p>
    </span>
    $footer:noescape()$
    $legalparagraph()$
#####
remindMailNotSignedOrderContent=
    $header:noescape()$
    <span>
      <p><i>$_remindMailNotSignedOrderContentInfo()$</i><br/>
        $_remindMailNotSignedOrderContentPartys()$ $partnersinfo:noescape()$<br/>
        $whohadsignedorderinfo:noescape()$<br/> 
        $timetosigninfo:noescape()$
        $appendixList()$
      </p>
      <p>$_remindMailNotSignedOrderContentInstruction()$</p>
    </span>
    $footer:noescape()$
    $legalparagraph()$
###
signupLinkEmailPart=
    <p>$_signupLinkEmailPartUserName()$ $email$</p>
    
    <p>$_signupLinkEmailPartClickLink()$<a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$

####
newUserMailTitle=$_newUserMailTitle()$
####
newUserMailContent=
<p>$_newUserMailContentHi()$</p>
<p>$_newUserMailContentBodyFirst()$</p>
<p>$_newUserMailContentBodySecond()$</p>
<p>$_newUserMailContentBodyUserName()$ $email$

</p>

<p>$_newUserMailContentBodyThird()$ <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a>

</p>

$poweredBySkrivaPaPara()$
####
inviteSubaccountMailTitle=$_inviteSubaccountMailTitle()$
####
inviteSubaccountMailContent=
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_inviteSubaccountMailContentBodyFirst()$</p>

    <p>$_inviteSubaccountMailContentBodyUserName()$<br/>
    $email$</p>
    
    <p>$_inviteSubaccountMailContentClickLink()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$
####
mailAccountBySigningContractReminderTitle=$_mailAccountBySigningContractReminderTitle()$
###
mailAccountBySigningContractReminderContent=
<p>$_hi()$ <strong>$personname$</strong>,</p>

<p>$_mailAccountBySigningContractReminderContentBody()$</p>

<p>$_mailAccountBySigningContractReminderContentInstruction()$</p>

$poweredBySkrivaPaPara()$
####
mailAccountBySigningOfferReminderTitle= $_mailAccountBySigningOfferReminderTitle()$
###
mailAccountBySigningOfferReminderContent=
<p>$_hi()$ <strong>$personname$</strong>,</p>

<p>$_mailAccountBySigningOfferReminderContentBody()$</p>

<p>$_mailAccountBySigningOfferReminderContentInstruction()$</p>

$poweredBySkrivaPaPara()$
###
mailViralInviteTitle=$_mailViralInviteTitle()$
###
mailViralInviteContent=
    <p>$_hi()$,</p>

    <p>$_mailViralInviteContentBodyFirst()$</p>

    <p>$_mailViralInviteContentBodySecond()$</p>

    <p>$_mailViralInviteContentBodyUserName()$$email$</p>
    
    <p>$_mailViralInviteContentBodyThird()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$

####
mailNewAccountCreatedByAdminTitle=$_mailNewAccountCreatedByAdminTitle()$
####
mailNewAccountCreatedByAdminContentHeader=
 
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_mailNewAccountCreatedByAdminContentHeaderBody()$</p>

###
mailNewAccountCreatedByAdminContent=
   $if(custommessage)$
    $custommessage:noescape()$
    $else$
    $mailNewAccountCreatedByAdminContentHeader()$
    $endif$
   
    <p>$_mailNewAccountCreatedByAdminContentUserName()$ $email$</p>
    
    <p>$_mailNewAccountCreatedByAdminContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$
####
invitationMailDeferredTitle=$_invitationMailDeferredTitle()$
###
invitationMailDeferredContent=
    <span>
      <p>$_hi()$ $authorname$,</p>
      <p>$_invitationMailDeferredContentBodyFirst()$</p>
      <p>$_invitationMailDeferredContentBodySecond()$<a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a></p>
      $poweredBySkrivaPaPara()$
    </span>
###
invitationMailDeliveredAfterDeferredTitle=$_invitationMailDeliveredAfterDeferredTitle()$
###
invitationMailDeliveredAfterDeferredContent=
    <span>
      <p>$_hi()$ $authorname$,</p>
      <p>$_invitationMailDeliveredAfterDeferredContentBody()$</p>
      $poweredBySkrivaPaPara()$
    </span>
###
invitationMailUndeliveredTitle=$_invitationMailUndeliveredTitle()$
###
invitationMailUndeliveredContent=
    <span>
      <p>$_hi()$ $authorname$,</p>
       <p>$_invitationMailUndeliveredContentBody()$</p>
       <p>$_invitationMailUndeliveredContentInstruction()$</p>
      $poweredBySkrivaPaPara()$
    </span>
####
mailDocumentErrorTitle=$_mailDocumentErrorTitle()$
###
mailDocumentErrorContent=
    <p>$_hi()$,</p>
    <p>$_mailDocumentErrorContentBodyFirst()$</p>
    <p>$_mailDocumentErrorContentBodySecond()$</p>
    <p>$_mailDocumentErrorContentBodyThird()$</p>
    <p>$_mailDocumentErrorContentBodyFourth()$</p>
    $poweredBySkrivaPaPara()$
###
passwordChangeLinkMailTitle= $_passwordChangeLinkMailTitle()$
###
passwordChangeLinkMailContent=
    <p>$_hi()$ $personname$,</p>
    <p>$_passwordChangeLinkMailContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>
    $poweredBySkrivaPaPara()$
###
###
####UTILS
timetosigninfo=
    <span>$_timetosigninfo()$ <strong>$time$</strong>.</span>
###
updateinglistwithauthor=
    <span>
      <span class="partylistupdate"></span>
    </span>
####
noonehadsignedtext=$_noonehadsignedtext()$####
contracthassignedformailtext=$_contracthassignedformailtext()$
####
offerhassignedformailtext=$_offerhassignedformailtext()$
####
orderhassignedformailtext=$_orderhassignedformailtext()$
####
whohadsignedcontractinfoformail=
    $if(signedlist)$	$_whohadsignedcontractinfoformailIfSigned()$ $signedlist:noescape()$
    $else$	$_whohadsignedcontractinfoformailIfNotSigned()$ $noonehadsignedtext()$
    $endif$
####
whohadsignedorderinfoformail=
    $if(signedlist)$	$_whohadsignedorderinfoformailIfAccepted()$ $signedlist:noescape()$
    $else$	$_whohadsignedorderinfoformailIfNotAccepted()$ $noonehadsignedtext()$
    $endif$
####
whohadsignedofferinfoformail=
    $if(signedlist)$	$_whohadsignedofferinfoformailIfConfirmed()$$signedlist:noescape()$
    $else$	$_whohadsignedofferinfoformailIfNotConfirmed()$$noonehadsignedtext()$
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
mailMismatchSignatoryTitle=$_mailMismatchSignatoryTitle()$
####
mailMismatchSignatoryContent=
<p>$_hi()$ $signame$,</p>

<p>$_mailMismatchSignatoryContentBodyFirst()$</p>

<p>$_mailMismatchSignatoryContentBodySecond()$</p>

$if(messages)$
<p>$_mailMismatchSignatoryContentBodyIfMessages()$</p>
$messages:noescape()$
$endif$

<p>$_mailMismatchSignatoryContentBestRegards()$</p>
<p>$_skrivapa()$</p>
####
mailMismatchAuthorTitle=$_mailMismatchAuthorTitle()$
####
mailMismatchAuthorContent=
<p>$_hi()$ $authorname$,</p>

<p>$_mailMismatchAuthorContentBodyFirst()$</p>

<p>$_mailMismatchAuthorContentBodySecond()$</p>
 
<p>$_mailMismatchAuthorContentErrorReport()$</p>

$messages:noescape()$

<p>$_mailMismatchAuthorContentBestRegards()$</p>
<p>$_skrivapa()$</p>
####
mailInviteUserAsSubaccountTitle=$_mailInviteUserAsSubaccountTitle()$
###
mailInviteUserAsSubaccountContent=
  <p>$_hi()$ $invited.fullnameOrEmail$,</p>
  <p>$_mailInviteUserAsSubaccountContentBodyFirst()$</p>

 <p>$_mailInviteUserAsSubaccountContentBodySecond()$</p>

 <p>$_mailInviteUserAsSubaccountContentInstructionFirst()$</p>
 <ul>$_mailInviteUserAsSubaccountContentInstructionSecond()$
</ul>

 <p><a href="$hostpart$/account/bsa/$supervisor.id$">$_mailInviteUserAsSubaccountContentLink()$</a></p>
  $poweredBySkrivaPaPara()$
####
####
mailSubaccountAcceptedTitle=$_mailSubaccountAcceptedTitle()$
###
mailSubaccountAcceptedContent=
  <p>$_hi()$ $user.fullnameOrEmail$,</p>
  <p>$_mailSubaccountAcceptedContentBody()$</p>
  $poweredBySkrivaPaPara()$
####
appendixList=$if(isattachments)$ $_appendixListAttachment()$ $attachments:{a | $a$ };separator=", "$ <br />$endif$
###
sigattachlist=$if(hassigattachments)$ $_sigattachlistRequestedAttachment()$ $sigattachments:{sa|$sa.name:strong()$ från $sa.sigs:noescape()$};separator="; "$ <br />$endif$
###
