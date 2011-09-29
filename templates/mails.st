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
    $header:noescape()$
    <p>$_mailCancelContractByAuthorContentBody()$</p>
    $footer:noescape()$
#####
mailCancelOfferByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $header:noescape()$
    <p>$_mailCancelOfferByAuthorContentBody()$</p>
    $footer:noescape()$
#####
mailCancelOrderByAuthor=
    $_mailCancelDocumentByAuthorTitle()$
    $header:noescape()$
    <p>$_mailCancelOrderByAuthorContentBody()$</p>
    $footer:noescape()$
#####
mailDocumentAwaitingForAuthorContent=
    $_mailDocumentAwaitingForAuthorTitle()$
    <p>$_hi()$,</p>
    <p>$_mailDocumentAwaitingForAuthorContentSignedBy()$</p>
    <p>$_mailDocumentAwaitingForAuthorContentInstruction()$</p>
    $footer:noescape()$
#####
mailContractClosed=
    $_mailDocumentClosedTitle()$
    <span>
      <p>$_hi()$,</p>
      <p>$_mailContractClosedContentSignedBy()$</p>
      <p>$_mailContractClosedContentAttached()$</p> 
      $footer:noescape()$
    </span>
####
mailOfferClosed=
    $_mailDocumentClosedTitle()$
    <span>
      <p>$_hi()$,</p>
      <p>
       $if(!service)$
        $_mailOfferClosedContentOfferAccepted()$
       $else$
        $_mailOfferClosedContentOfferAcceptedWithService()$
       $endif$
       </p>
      <p>$_mailOfferClosedContentAttached()$</p>
      $footer:noescape()$
    </span>
####
mailOrderClosed=
    $_mailDocumentClosedTitle()$
    <span>
      <p>$_hi()$,</p>

      <p>
        
       $if(!service)$
        $_mailOrderClosedContentViewed()$
       $else$
        $_mailOrderClosedContentViewedWithService()$
       $endif$
      </p>
      <p>$_mailOrderClosedContentAttached()$</p>
      $footer:noescape()$
    </span>
####
mailRejectContractMail=
      $_mailRejectMailTitle()$
      $mailRejectMailContentHeader()$
      <p>$_mailRejectContractMailContentContractRejected()$</p>
      $footer:noescape()$
####
mailRejectOfferMail=
      $_mailRejectMailTitle()$
      $mailRejectMailContentHeader()$
      <p>$_mailRejectOfferMailContentOfferRejected()$</p>
      $footer:noescape()$
###
mailRejectOrderMail=
      $_mailRejectMailTitle()$
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
    $header:noescape()$
    <p>
     <i>$_mailInvitationToSignContractContentInfo()$</i><br/>
     $_mailInvitationToSignContractContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedinfo:noescape()$<br/>
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
mailInvitationToSignOffer=
    $mailInvitationTitle()$
    $header:noescape()$
    <p>
     <i>$_mailInvitationToSignOfferContentInfo()$</i><br/>
     $_mailInvitationToSignOfferContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedinfo:noescape()$<br/>
     $_mailInvitationToSignOfferContentSender()$ <strong>$creatorname$</strong><br/>
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
mailInvitationToSignOrder=
    $mailInvitationTitle()$
    $header:noescape()$
    <p>
     <i>$_mailInvitationToSignOrderContentInfo()$</i><br/>
     $_mailInvitationToSignOrderContentPartys()$ $partnersinfo:noescape()$<br/>
     $whohadsignedinfo:noescape()$<br/>
     $_mailInvitationToSignOrderContentSender()$ <strong>$creatorname$</strong><br/>
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
####
remindMailSigned=
    $documenttitle$
    $header:noescape()$
    $footer:noescape()$
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
####
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
remindMailNotSignedOffer=
    $_remindMailNotSignedTitle()$
    $header:noescape()$
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
    $footer:noescape()$
    $legalparagraph()$
#####
remindMailNotSignedOrder=
    $_remindMailNotSignedTitle()$
    $header:noescape()$
    <span>
      <p><i>$_remindMailNotSignedOrderContentInfo()$</i><br/>
        $_remindMailNotSignedOrderContentPartys()$ $partnersinfo:noescape()$<br/>
        $whohadsignedinfo:noescape()$<br/> 
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
    
    <p>$_signupLinkEmailPartClickLink()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$

####
newUserMail=
$_newUserMailTitle()$
<p>$_hi()$,</p>
<p>$_newUserMailContentBodyFirst()$</p>
<p>$_newUserMailContentBodySecond()$</p>
<p>$_newUserMailContentBodyUserName()$ $email$

</p>

<p>$_newUserMailContentBodyThird()$ <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a>

</p>

$poweredBySkrivaPaPara()$
####
inviteCompanyAccountMail=
    $_inviteCompanyAccountMailTitle()$
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_inviteCompanyAccountMailContentBodyFirst()$</p>

    <p>$_inviteCompanyAccountMailContentBodyUserName()$<br/>
    $email$</p>
    
    <p>$_inviteCompanyAccountMailContentClickLink()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$
####
mailAccountBySigningContractReminder=
$_mailAccountBySigningContractReminderTitle()$
<p>$_hi()$ <strong>$personname$</strong>,</p>

<p>$_mailAccountBySigningContractReminderContentBody()$</p>

<p>$_mailAccountBySigningContractReminderContentInstruction()$</p>

$poweredBySkrivaPaPara()$
###
mailAccountBySigningOfferReminder=
$_mailAccountBySigningOfferReminderTitle()$
<p>$_hi()$ <strong>$personname$</strong>,</p>

<p>$_mailAccountBySigningOfferReminderContentBody()$</p>

<p>$_mailAccountBySigningOfferReminderContentInstruction()$</p>

$poweredBySkrivaPaPara()$
###
mailAccountBySigningOrderReminder=
 $_mailAccountBySigningOrderReminderTitle()$
<p>$_hi()$ <strong>$personname$</strong>,</p>

<p>$_mailAccountBySigningOrderReminderContentBody()$</p>

<p>$_mailAccountBySigningOrderReminderContentInstruction()$</p>

$poweredBySkrivaPaPara()$
###
mailViralInvite=
    $_mailViralInviteTitle()$
    <p>$_hi()$,</p>

    <p>$_mailViralInviteContentBodyFirst()$</p>

    <p>$_mailViralInviteContentBodySecond()$</p>

    <p>$_mailViralInviteContentBodyUserName()$ $email$</p>
    
    <p>$_mailViralInviteContentBodyThird()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$

####
mailNewAccountCreatedByAdminContentHeader=
    <p>$_hi()$ <strong>$personname$</strong>,</p>

    <p>$_mailNewAccountCreatedByAdminContentHeaderBody()$</p>

###
mailNewAccountCreatedByAdmin=
   $_mailNewAccountCreatedByAdminTitle()$
   $if(custommessage)$
    $custommessage:noescape()$
    $else$
    $mailNewAccountCreatedByAdminContentHeader()$
    $endif$
   
    <p>$_mailNewAccountCreatedByAdminContentUserName()$ $email$</p>
    
    <p>$_mailNewAccountCreatedByAdminContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$
####
invitationMailDeferred=
    $_invitationMailDeferredTitle()$
    <span>
      <p>$_hi()$ $authorname$,</p>
      <p>$_invitationMailDeferredContentBodyFirst()$</p>
      <p>$_invitationMailDeferredContentBodySecond()$ <a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a></p>
      $poweredBySkrivaPaPara()$
    </span>
###
invitationMailDeliveredAfterDeferred=
    $_invitationMailDeliveredAfterDeferredTitle()$
    <span>
      <p>$_hi()$ $authorname$,</p>
      <p>$_invitationMailDeliveredAfterDeferredContentBody()$</p>
      $poweredBySkrivaPaPara()$
    </span>
###
invitationMailUndelivered=
    $_invitationMailUndeliveredTitle()$
    <span>
      <p>$_hi()$ $authorname$,</p>
       <p>$_invitationMailUndeliveredContentBody()$</p>
       <p>$_invitationMailUndeliveredContentInstruction()$</p>
      $poweredBySkrivaPaPara()$
    </span>
####
mailDocumentError=
    $_mailDocumentErrorTitle()$
    <p>$_hi()$,</p>
    <p>$_mailDocumentErrorContentBodyFirst()$</p>
    <p>$_mailDocumentErrorContentBodySecond()$</p>
    <p>$_mailDocumentErrorContentBodyThird()$</p>
    <p>$_mailDocumentErrorContentBodyFourth()$</p>
    $poweredBySkrivaPaPara()$
###
passwordChangeLinkMail=
    $_passwordChangeLinkMailTitle()$
    <p>$_hi()$ $personname$,</p>
    <p>$_passwordChangeLinkMailContentBody()$ <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>
    $poweredBySkrivaPaPara()$
###
###
####UTILS
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
    $else$	$_whohadsignedofferinfoformailIfNotConfirmed()$ $noonehadsignedtext()$
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
mailMismatchSignatory=
$_mailMismatchSignatoryTitle()$
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
mailMismatchAuthor=
$_mailMismatchAuthorTitle()$
<p>$_hi()$ $authorname$,</p>

<p>$_mailMismatchAuthorContentBodyFirst()$</p>

<p>$_mailMismatchAuthorContentBodySecond()$</p>
 
<p>$_mailMismatchAuthorContentErrorReport()$</p>

$messages:noescape()$

<p>$_mailMismatchAuthorContentBestRegards()$</p>
<p>$_skrivapa()$</p>
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
mailCompanyAccountAcceptedTitle=$_mailCompanyAccountAcceptedTitle()$
###
mailCompanyAccountAcceptedContent=
  <p>$_hi()$ $user.fullnameOrEmail$,</p>
  <p>$_mailCompanyAccountAcceptedContentBody()$</p>
  $poweredBySkrivaPaPara()$
####
appendixList=$if(isattachments)$ $_appendixListAttachment()$ $attachments:{a | $a$ };separator=", "$ <br />$endif$
###
sigattachlist=$if(hassigattachments)$ $_sigattachlistRequestedAttachment()$ $sigattachments:{sa|$sa.name:strong()$ $_from()$ $sa.sigs:noescape()$};separator="; "$ <br />$endif$
###
mailMailAPIConfirmContract= 
   $_mailMailAPIConfirmTitle()$
   $_mailMailAPIConfirmContractHeader()$
    <p>
     <i>$_mailInvitationToSignContractContentInfo()$</i><br/>
     $_mailInvitationToSignContractContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignContractContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
    </p>
    $footer:noescape()$
####
mailMailAPIConfirmOrder=
   $_mailMailAPIConfirmTitle()$ 
   $_mailMailAPIConfirmOrderHeader()$
   <p>
     <i>$_mailInvitationToSignOrderContentInfo()$</i><br/>
     $_mailInvitationToSignOrderContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignOrderContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
    </p>
    $footer:noescape()$
####
mailMailAPIConfirmOffer= 
   $_mailMailAPIConfirmTitle()$
   $_mailMailAPIConfirmOfferHeader()$
    <p>
     <i>$_mailInvitationToSignOfferContentInfo()$</i><br/>
     $_mailInvitationToSignOfferContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignOfferContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
    </p>
    $footer:noescape()$
####
