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
mailDocumentAwaitingForAuthorContentMainP =
   $_hi()$ <strong>$authorname$</strong>,
#####
##### CLOSED MAIL
#####
mailContractClosed=
    $_mailDocumentClosedTitleContract()$
    $nicemail(
        mailheader = _mailDocumentClosedTitleContract();
        mailp1 = mailContractClosedMainP();
        mailp2 = regards();
        footer = footer
    )$

####
mailContractClosedMainP =
$_hi()$ <strong>$signatoryname$</strong>,
<br/>
<br/>
      $_mailContractClosedContentSignedBy()$
      <br/>
      <br/>
      $_mailContractClosedContentAttached()$ $mailClosedSignup()$
####
mailOfferClosed=
    $_mailDocumentClosedTitleOffer()$
    $nicemail(
        mailheader = _mailDocumentClosedTitleOffer();
        mailp1 = mailContractClosedMainP();
        mailp2 = regards();
        footer = footer
    )$
####
mailOfferClosedMainP =
      $_mailOfferClosedContentOfferAccepted()$
    <br/>
    <br/>
    $_mailOfferClosedContentAttached()$ $mailClosedSignup()$
####
mailOrderClosed=
    $_mailDocumentClosedTitleOrder()$
    $nicemail(
        mailheader = _mailDocumentClosedTitleOrder();
        mailp1 = mailContractClosedMainP();
        mailp2 = regards();
        footer = footer
    )$
####
mailOrderClosedMainP=
        $_mailOrderClosedContentViewed()$
      <br/>
      <br/>
      $_mailOrderClosedContentAttached()$ $mailClosedSignup()$
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
mailInvitationToSignContractP1=
    $nicemailbodywithpreview(
        hidetitle='true';

        r1='true';
        r1cond='true';
        r1elems=header;
        r1noescp='true';

        r2='true';
        r2cond='true';
        r2elems=mailInvitationToSignContractIntructions();
        r2noescp='true'
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
mailInvitationToSignOfferP1=
    $nicemailbodywithpreview(
        hidetitle='true';

        r1='true';
        r1cond='true';
        r1elems=header;
        r1noescp='true';

        r2='true';
        r2cond='true';
        r2elems=mailInvitationToSignOfferIntructions();
        r2noescp='true'
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
mailInvitationToSignOrderP1=
    $nicemailbodywithpreview(
        hidetitle='true';

        r1='true';
        r1cond='true';
        r1elems=header;
        r1noescp='true';

        r2='true';
        r2cond='true';
        r2elems=mailInvitationToSignOrderIntructions();
        r2noescp='true'
    )$
###
mailInvitationToSignOrderIntructions=
    $nicebutton(link=link; text=_mailReviewOnlineDocument())$
####
mailInvitationToSignContractDefaultHeader=
$_mailInvitationToSignContractDefaultHeader()$
<br/>
<div class="editable" name="customtext">
</div>
####
mailInvitationToSignOfferDefaultHeader=
$_mailInvitationToSignOfferDefaultHeader()$
<br/>
<div class="editable" name="customtext">
</div>
####
mailInvitationToSignOrderDefaultHeader=
$_mailInvitationToSignOrderDefaultHeader()$
<br/>
<div class="editable" name="customtext">
</div>
####
mailInvitationToViewDefaultHeader=
$_hi()$ <strong>$personname$</strong>,
<br/>
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
$_hi()$ <strong>$personname$</strong>,
<br/>
<br/>
  $_remindMailSignedContractStandardHeaderSendRequest()$
  <br/>
  <br/>
  $_remindMailSignedContractStandardHeaderSendRequest2()$
####
remindMailSignedOfferStandardHeader=
$_hi()$ <strong>$personname$</strong>,
<br/>
<br/>
  $_remindMailSignedOfferStandardHeaderSendRequest()$
<br/>
<br/>
$regards()$
####
remindMailSignedOrderStandardHeader=
$_hi()$ <strong>$personname$</strong>,
<br/>
<br/>
  $_remindMailSignedOrderStandardHeaderSendRequest()$
<br/>
<br/>
$regards()$
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
        mailp1=_newUserMailHeader();
        mailp2=newUserMailMainP();
        mailp3=regards()
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
    $nicemail(
        mailheader=_mailNewAccountCreatedByAdminTitle();
        mailp1=mailNewCompanyUserInviteMainP();
        mailp2=regards()
    )$
####
mailNewCompanyUserInviteMainP=
 $_hi()$ <strong>$invitedname$</strong>,
 <br/>
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
    $_hi()$ <strong>$personname$</strong>,
    <br/>
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
        mailp2=mailNewAccountCreatedByAdminMainP();
        mailp3=regards()
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
        mailp1=invitationMailDeferredMainP();
        mailp2=regards()
    )$
###
invitationMailDeferredMainP=
      $_hi()$ <strong>$authorname$</strong>,
      <br/>
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
        mailp1=invitationMailDeliveredAfterDeferredMainP();
        mailp2=_invitationMailDeliveredAfterDeferredContentBody();
        mailp3=regards()
    )$
###
invitationMailDeliveredAfterDeferredMainP =
      $_hi()$ <strong>$authorname$</strong>,
###
### UNDELIVERED MAIL
###
invitationMailUndelivered=
    $_invitationMailUndeliveredTitle()$
    $nicemail(
        mailheader=_invitationMailUndeliveredTitle();
        mailp1=invitationMailUndeliveredMainP();
        mailp2=regards()
    )$
####
####
invitationMailUndeliveredMainP =
       $_hi()$ <strong>$authorname$</strong>,
       <br/>
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
        mailp1=mailDocumentErrorMainP();
        mailp2=_mailDocumentErrorContentBodyFirst();
        mailp3=_mailDocumentErrorContentBodySecond();
        mailp4=_mailDocumentErrorContentBodyThird();
        mailspacing4='';
        mailp5=regards()
    )$
###
mailDocumentErrorMainP =
    $_hi()$ <strong>$creatorname$</strong>,
####
#### CHANGE PASSWORD MAIL
####
passwordChangeLinkMail=
    $_passwordChangeLinkMailTitle()$
    $nicemail(
        mailheader=_passwordChangeLinkMailTitle();
        mailp1=passwordChangeLinkMailMainP();
        mailp2=regards()
    )$
###
passwordChangeLinkMailMainP=
$_hi()$ <strong>$personname$</strong>,
<br/>
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
        mailp1=mailMismatchSignatoryMainP();
        mailp2=regards()
    )$
#####
mailMismatchSignatoryMainP=
$_hi()$ <strong>$signame$</strong>,
<br/>
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
        mailp1=mailMismatchAuthorMainP();
        mailp2=regards()
    )$
####
mailMismatchAuthorMainP =
$_hi()$ <strong>$authorname$</strong>,
<br/>
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
      mailp1=mailTakeoverPrivateUserInviteP();
      mailp2=regards()
    )$
####
mailTakeoverPrivateUserInviteP=
  $_hi()$ <strong>$invitedname$</strong>,
  <br/>
  <br/>
  $_mailTakeoverPrivateUserInviteContentBodyFirst()$
  <br/>
  <br/>
  $_mailTakeoverPrivateUserInviteContentBodySecond()$
  <br/>
  <br/>
  $_mailTakeoverPrivateUserInviteContentInstructionFirst()$
  <ul>
    $_mailTakeoverPrivateUserInviteContentInstructionSecond()$
  </ul>
  $_mailTakeoverPrivateUserInviteContentLink()$ $_mailTakeoverPrivateUserInviteContentInstructionThird()$ <a href="$ctxhostpart$$link$">$ctxhostpart$$link$</a>
####
mailRequestChangeEmail=
  $_mailRequestChangeEmailTitle()$
  $nicemail(
    mailheader=_mailRequestChangeEmailTitle();
    mailp1=mailRequestChangeEmailP();
    mailp2=regards()
  )$
####
mailRequestChangeEmailP=
  $_hi()$ <strong>$fullname$</strong>,
  <br/>
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
$_mailMailAPIConfirmContractHeader()$
     <br/>
     <br/>
     <i>$_mailInvitationToSignContractContentInfo()$</i><br/>
     $_mailInvitationToSignContractContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignContractContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
####
mailMailAPIConfirmOrder=
   $_mailMailAPIConfirmTitle()$
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=_mailMailAPIConfirm()
    )$
####
mailMailAPIConfirmOrderMainP=
$_mailMailAPIConfirmOrderHeader()$
     <br/>
     <br/>
     <i>$_mailInvitationToSignOrderContentInfo()$</i><br/>
     $_mailInvitationToSignOrderContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignOrderContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
####
mailMailAPIConfirmOffer=
   $_mailMailAPIConfirmTitle()$
   $nicemail(
        mailheader=_mailMailAPIConfirmTitle();
        mailp1=_mailMailAPIConfirm()
    )$
####
mailMailAPIConfirmOfferMainP=
    $_mailMailAPIConfirmOfferHeader()$
    <br/>
    <br/>
     <i>$_mailInvitationToSignOfferContentInfo()$</i><br/>
     $_mailInvitationToSignOfferContentPartys()$ $partnersinfo:noescape()$<br/>
     $_mailInvitationToSignOfferContentSender()$ <strong>$creatorname$</strong><br/>
     $timetosigninfo:noescape()$
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
nicemailbodywithpreview=
<table style="margin: auto; width:540px;">
    <tr>
       <td>
          <div style='text-align:center;min-width:190px'>
            <div style="display:inline-block;margin:auto;max-width:190px;max-height:270px;min-width:95px;min-height:135px;border:1px solid #CCCCCC">
               <img src="$ctxhostpart$$previewLink$" alt='' style='margin:0px;padding:0px;border:0px;'/>
             </div>
          </div>
       </td>
       <td>

        <table style="font-family:'Lucida Grande', sans-serif;font-size:12px;margin: auto;display: block; max-width: 330px; overflow: auto;">
            $if(!hidetitle)$
            <tr>
                <td><span style="color:#212121;font-size:1.5em;">$documenttitle$</span></td>
            </tr>
            $endif$
               $if(r1)$
                $nicemailpreviewrow(
                    cond=r1cond;
                    text= r1text;
                    elems=r1elems;
                    alter=r1alter;
                    alttext = r1alttext;
                    altelems = r1altelems;
                    elemsclass = r1elemsclass;
                    separator = r1separator;
                    noescp = r1noescp
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
                    separator = r2separator;
                    noescp = r2noescp
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
                    separator = r3separator;
                    noescp = r3noescp
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
                    separator = r4separator;
                    noescp = r4noescp
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
                    separator = r5separator;
                    noescp = r5noescp
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
                    separator = r6separator;
                    noescp = r6noescp
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
                    separator = r7separator;
                    noescp = r7noescp
                )$
            $endif$
        </table>
       </td>
   </tr>
</table>
####
nicemailpreviewrow=
$if(cond)$
<tr>
    $if(noescp)$
    <td>
      <table width="290px" style="font-family:'Lucida Grande', sans-serif;font-size:12px;margin-left:30px">
      <tbody><tr><td>$elems:noescape()$</td></tr></tbody>
      </table>
    </td>
    $else$
    <td>
      <p style="font-style:italic;font-family:sans-serif;vertical-align:top;font-size:11px;">$text$</p>
          $elems: {elem |
              $if(elem.0)$
                <p style="font-weight:bold;"> $elem.0$ $separator$ $elem.1$</p>
              $else$
                <p style="font-weight:bold;" $if(elemsclass)$class="$elemsclass$"$endif$> $elem$ </p>
              $endif$
          }$
    </td>
    $endif$
</tr>
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
        mailheader= "Scrive by Mail Error";
        mailp1 = "<strong>Greetings from Scrive by Mail,</strong>";
        mailp2 = "We were unable to process the mail you sent us and therefore have not forwarded the document to anyone.";
        mailp3 = errormsg;
        mailp4 = "Thanks for using Scrive by Mail"
    )$
#####
mailMailAPIDelayAdmin=
    Scrive by Mail User Confirmation
    $nicemail(
        mailheader= "Scrive by Mail User Confirmation";
        mailp1 = "<strong>Greetings from Scrive by Mail,</strong>";
        mailp2 = concat(a1="We have just received an email from ";a2=email;a3=" which is an account on your domain, requesting to be added to your Scrive account.");
        mailp3 = "If this is a valid email address within your company or organization, please click on the confirmation link below and we will create the new account. This link expires in three days for security reasons.";
        mailp4 = linkk(href=confirmationlink;text="Click here to confirm this account.");
        mailp5 = "If you received this email in error or if the email address is incorrect, please ignore this email.<br/><br/>Thanks for using Scrive by Mail."
    )$
#####
mailMailAPIDelayUser=
    Scrive by Mail
    $nicemail(
        mailheader= "Scrive By Mail";
        mailp1 = "<strong>Greetings from Scrive by Mail,</strong>";
        mailp2 = "We were unable to immediately process the mail you sent us and therefore have not forwarded the document to anyone.";
        mailp3 = "At this point in time, there is no account associated with this email address. We have contacted the Company Account administrators and asked them to confirm the creation of your new account. Upon creation of your account, we will continue processing the document. If you have not received an invitation within three days, please contact the administrator of the Company Account.";
        mailp4 = "Thanks for using Scrive by Mail"
    )$
#####
para=<p>$a$</p>
#####
linkk=<a href="$href$">$text$</a>
####
concat=$a1:noescape()$$a2:noescape()$$a3:noescape()$
####
regards=
$_regards()$,<br/>
Scrive
