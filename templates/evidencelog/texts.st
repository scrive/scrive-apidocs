AttachAttachExtendedSealedFileEvidenceTextFull=The document was sealed with a keyless digital signature by Scrive E-sign.
###
AttachExtendedSealedFileEvidenceText=*
$if(full)$
  $AttachAttachExtendedSealedFileEvidenceTextFull()$
$else$
  $_AttachExtendedSealedFileEvidenceTextSimple()$
$endif$
###
AttachGuardtimeSealedFileEvidenceTextFull=The document was sealed with a digital signature by Scrive E-sign.
###
AttachGuardtimeSealedFileEvidenceText=*
$if(full)$
  $AttachGuardtimeSealedFileEvidenceTextFull()$
$else$
  $_AttachGuardtimeSealedFileEvidenceTextSimple()$
$endif$
###
AttachSealedFileEvidenceTextFull=Scrive E-sign prepared the final PDF, including evidence collected by Scrive E-sign, before sealing with a digital signature.
###
AttachSealedFileEvidenceText=*
$if(full)$
  $AttachSealedFileEvidenceTextFull()$
$else$
  $if(archive)$
    $_AttachSealedFileEvidenceTextSimpleArchive()$
  $else$
    $_AttachSealedFileEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
AutomaticReminderSentTextFullEmail=An automatic reminder to sign was sent to $signatory$ using email at the request of $author$.
###
AutomaticReminderSentTextFullSMS=An automatic reminder to sign was sent to $signatory$ using SMS at the request of $author$.
###
AutomaticReminderSentTextFullEmailSMS=An automatic reminder to sign was sent to $signatory$ using email and SMS at the request of $author$.
###
AutomaticReminderSentText=*
$if(full)$
  $if(email)$
    $AutomaticReminderSentTextFullEmail()$
  $elseif(mobile)$
    $AutomaticReminderSentTextFullSMS()$
  $elseif(emailmobile)$
    $AutomaticReminderSentTextFullEmailSMS()$
  $endif$
$else$
  $if(email)$
    $_AutomaticReminderSentTextSimpleEmail()$
  $elseif(mobile)$
    $_AutomaticReminderSentTextSimpleSMS()$
  $elseif(emailmobile)$
    $_AutomaticReminderSentTextSimpleEmailSMS()$
  $endif$
$endif$
###
CancelDocumentEvidenceTextFull=The invitation to sign was withdrawn by $actor$. The possibility to sign was disabled by Scrive E-sign.
###
CancelDocumentEvidenceText=*
$if(full)$
  $CancelDocumentEvidenceTextFull()$
$else$
  $if(archive)$
    $_CancelDocumentEvidenceTextSimpleArchive()$
  $else$
    $_CancelDocumentEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
ChangeSignatoryEmailWhenUndeliveredEvidenceText=The email address for the signatory was changed from $oldemail$ to $newemail$ by $actor$.
###
ChangeSignatoryPhoneWhenUndeliveredEvidenceText=The phone number for the signatory was changed from $oldphone$ to $newphone$ by $actor$.
###
ChangeAuthenticationMethodStandardToELegEvidenceText=$actor$ changed authentication for $signatory$ from “no extra authentication” to “eID”.
###
ChangeAuthenticationMethodStandardToSMSEvidenceText=$actor$ changed authentication for $signatory$ from “no extra authentication” to “PIN by SMS”.
###
ChangeAuthenticationMethodELegToStandardEvidenceText=$actor$ changed authentication for $signatory$ from “eID” to “no extra authentication”.
###
ChangeAuthenticationMethodELegToSMSEvidenceText=$actor$ changed authentication for $signatory$ from “eID” to “PIN by SMS”.
###
ChangeAuthenticationMethodSMSToStandardEvidenceText=$actor$ changed authentication for $signatory$ from “PIN by SMS” to “no extra authentication”.
###
ChangeAuthenticationMethodSMSToElegEvidenceText=$actor$ changed authentication for $signatory$ from “PIN by SMS” to “eID”.
###
CloseDocumentEvidenceTextFull=Scrive E-sign locked the document from further modifications by the signing parties.
###
CloseDocumentEvidenceText=*
$if(full)$
  $CloseDocumentEvidenceTextFull()$
$else$
  $_CloseDocumentEvidenceTextSimple()$
$endif$
###
DeleteSigAttachmentEvidenceText=The attachment “$name$” previously uploaded by $actor$ on the request of $author$ was deleted by $actor$.
###
ErrorSealingDocumentEvidenceText=Due to a system error Scrive E-sign failed to finalise the document, i.e. failed to include the Verification Page, Evidence Log, Evidence of Intent, Evidence Documentation and Digital Signature Documentation into the document before sealing the Evidence Package (as defined in the Evidence Documentation) with a digital signature.
###
ReviewInvitationDeliveredByEmailText=Scrive E-sign’s external email delivery system reported that the invitation to review was delivered to $signatory$ by email.
###
SigningInvitationDeliveredByEmailText=Scrive E-sign’s external email delivery system reported that the invitation to sign was delivered to $signatory$ by email.
###
InvitationDeliveredByEmailText=*
$if(full)$
  $if(signing)$
    $SigningInvitationDeliveredByEmailText()$
  $elseif(viewing)$
    $ReviewInvitationDeliveredByEmailText()$
  $endif$
$else$
  $if(signing)$
    $_InvitationDeliveredByEmailTextToSignSimple()$
  $elseif(viewing)$
    $_InvitationDeliveredByEmailTextToReviewSimple()$
  $endif$
$endif$
###
ReviewInvitationDeliveredBySMSText=Scrive E-sign’s external SMS delivery system reported that the invitation to review was delivered to $signatory$ by SMS.
###
SigningInvitationDeliveredBySMSText=Scrive E-sign’s external SMS delivery system reported that the invitation to sign was delivered to $signatory$ by SMS.
###
InvitationDeliveredBySMSText=*
$if(full)$
  $if(signing)$
    $SigningInvitationDeliveredBySMSText()$
  $elseif(viewing)$
    $ReviewInvitationDeliveredBySMSText()$
  $endif$
$else$
  $if(signing)$
    $_InvitationDeliveredBySMSTextToSignSimple()$
  $elseif(viewing)$
    $_InvitationDeliveredBySMSTextToReviewSimple()$
  $endif$
$endif$
###
SigningInvitationEvidenceTextFullEmail=An invitation to sign was sent to $signatory$ using email. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToSignIfThereIsOne()$
###
SigningInvitationEvidenceTextFullSMS=An invitation to sign was sent to $signatory$ using SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToSignIfThereIsOne()$
###
SigningInvitationEvidenceTextFullEmailSMS=An invitation to sign was sent to $signatory$ using email and SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToSignIfThereIsOne()$
###
ReviewInvitationEvidenceTextFullEmail=An invitation to review was sent to $signatory$ using email. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToReviewIfThereIsOne()$
###
ReviewInvitationEvidenceTextFullSMS=An invitation to review was sent to $signatory$ using SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToReviewIfThereIsOne()$
###
ReviewInvitationEvidenceTextFullEmailSMS=An invitation to review was sent to $signatory$ using email and SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToReviewIfThereIsOne()$
###
InvitationEvidenceText=*
$if(signing)$
  $if(full)$
    $if(email)$
        $SigningInvitationEvidenceTextFullEmail()$
    $elseif(mobile)$
        $SigningInvitationEvidenceTextFullSMS()$
    $elseif(emailmobile)$
        $SigningInvitationEvidenceTextFullEmailSMS()$
    $endif$
  $else$
    $if(archive)$
      $if(email)$
            $_InvitationEvidenceTextToSignArchiveSimpleEmail()$
      $elseif(mobile)$
            $_InvitationEvidenceTextToSignArchiveSimpleSMS()$
      $elseif(emailmobile)$
            $_InvitationEvidenceTextToSignArchiveSimpleEmailSMS()$
      $endif$
    $else$
      $if(email)$
            $_InvitationEvidenceTextToSignNotArchiveSimpleEmail()$
      $elseif(mobile)$
            $_InvitationEvidenceTextToSignNotArchiveSimpleSMS()$
      $elseif(emailmobile)$
            $_InvitationEvidenceTextToSignNotArchiveSimpleEmailSMS()$
      $endif$
    $endif$
  $endif$
$elseif(viewing)$
  $if(full)$
    $if(email)$
        $ReviewInvitationEvidenceTextFullEmail()$
    $elseif(mobile)$
        $ReviewInvitationEvidenceTextFullSMS()$
    $elseif(emailmobile)$
        $ReviewInvitationEvidenceTextFullEmailSMS()$
    $endif$
  $else$
    $if(archive)$
      $if(email)$
            $_InvitationEvidenceTextToReviewArchiveSimpleEmail()$
      $elseif(mobile)$
            $_InvitationEvidenceTextToReviewArchiveSimpleSMS()$
      $elseif(emailmobile)$
            $_InvitationEvidenceTextToReviewArchiveSimpleEmailSMS()$
      $endif$
    $else$
      $if(email)$
            $_InvitationEvidenceTextToReviewNotArchiveSimpleEmail()$
      $elseif(mobile)$
            $_InvitationEvidenceTextToReviewNotArchiveSimpleSMS()$
      $elseif(emailmobile)$
            $_InvitationEvidenceTextToReviewNotArchiveSimpleEmailSMS()$
      $endif$
    $endif$
  $endif$
$endif$
###
InvitationUndeliveredByEmailTextFull=Scrive E-sign’s external email delivery system reported that the invitation to $signatory$ could not be delivered by email and continued attempts were cancelled.
###
InvitationUndeliveredBySMSTextFull=Scrive E-sign’s external SMS delivery system reported that the invitation could not be delivered by SMS and continued attempts were cancelled.
###
InvitationUndeliveredByEmailText=*
$if(full)$
  $InvitationUndeliveredByEmailTextFull()$
$else$
  $if(archive)$
    $_InvitationUndeliveredByEmailTextSimpleArchive()$
  $else$
    $_InvitationUndeliveredByEmailTextSimpleNotArchive()$
  $endif$
$endif$
###
InvitationUndeliveredBySMSText=*
$if(full)$
  $InvitationUndeliveredBySMSTextFull()$
$else$
  $if(archive)$
    $_InvitationUndeliveredBySMSTextSimpleArchive()$
  $else$
    $_InvitationUndeliveredBySMSTextSimpleNotArchive()$
  $endif$
$endif$
###
MarkSigningInvitationReadEvidenceText=Scrive E-sign’s external email delivery system reported that the invitation to sign (sent to $signatory_email$) was opened.
###
MarkReviewInvitationReadEvidenceText=Scrive E-sign’s external email delivery system reported that the invitation to review (sent to $signatory_email$) was opened.
###
MarkInvitationReadEvidenceText=*
$if(full)$
  $if(signing)$
    $MarkSigningInvitationReadEvidenceText()$
  $elseif(viewing)$
    $MarkReviewInvitationReadEvidenceText()$
  $endif$
$else$
  $if(signing)$
    $_MarkInvitationReadEvidenceTextToSignSimple()$
  $elseif(viewing)$
    $_MarkInvitationReadEvidenceTextToReviewSimple()$
  $endif$
$endif$
###
PreparationToPendingEvidenceTextFull=The preparation of the signing process settings was finished by $actor$, and Scrive E-sign locked the document from editing. The signing process language was set to $lang$, and the signing due date was set to $timeouttime$ UTC.$if(timezone)$ The time zone $timezone$ was provided by $actor$$endif$. The signing process was initiated.
###
PreparationToPendingEvidenceText=*
$if(full)$
  $PreparationToPendingEvidenceTextFull()$
$else$
  $if(archive)$
    $_PreparationToPendingEvidenceTextSimpleArchive()$
  $else$
    $_PreparationToPendingEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
ProlongDocumentEvidenceTextFull=The due date for signing was prolonged by $actor$.
###
ProlongDocumentEvidenceText=*
$if(archive)$
  $_ProlongDocumentEvidenceTextArchive()$
$else$
  $ProlongDocumentEvidenceTextFull()$
$endif$
###
RejectDocumentEvidenceTextFull=The invitation to sign was rejected by $actor$. The possibility to sign was disabled by Scrive E-sign.$if(text)$ The rejection message from $actor$ was: <BR/> <i>$text:noescape()$</i>$endif$
###
RejectDocumentEvidenceText=*
$if(full)$
  $RejectDocumentEvidenceTextFull()$
$else$
  $if(archive)$
    $_RejectDocumentEvidenceTextSimpleArchive()$
  $else$
    $_RejectDocumentEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
ReminderSendText=*
$if(archive)$
  $if(signing)$
    $if(email)$
        $_ReminderSendTextToSignArchiveEmail()$
    $elseif(mobile)$
        $_ReminderSendTextToSignArchiveSMS()$
    $elseif(emailmobile)$
        $_ReminderSendTextToSignArchiveEmailSMS()$
    $endif$
  $elseif(viewing)$
    $if(email)$
        $_ReminderSendTextToReviewArchiveEmail()$
    $elseif(mobile)$
        $_ReminderSendTextToReviewArchiveSMS()$
    $elseif(emailmobile)$
        $_ReminderSendTextToReviewArchiveEmailSMS()$
    $endif$
  $endif$
$else$
  $if(signing)$
    $if(email)$
        $_ReminderSendTextToSignNotArchiveEmail()$
    $elseif(mobile)$
        $_ReminderSendTextToSignNotArchiveSMS()$
    $elseif(emailmobile)$
        $_ReminderSendTextToSignNotArchiveEmailSMS()$
    $endif$
  $elseif(viewing)$
    $if(email)$
        $_ReminderSendTextToReviewNotArchiveEmail()$
    $elseif(mobile)$
        $_ReminderSendTextToReviewNotArchiveSMS()$
    $elseif(emailmobile)$
        $_ReminderSendTextToReviewNotArchiveEmailSMS()$
    $endif$
  $endif$
$endif$
###
RemovedFromPadDeviceText=The invitation to signatory $signatory$ to sign the document by in-person meeting was withdrawn by $actor$.
###
ResealedPDFText=Preparation of the Evidence Package was requested by $actor$, a person authorised to administer Scrive E-sign.
###
RestartDocumentEvidenceText=*
$if(full)$
  $RestartDocumentEvidenceTextFull()$
$else$
  $if(archive)$
    $_RestartDocumentEvidenceTextSimpleArchive()$
  $else$
    $_RestartDocumentEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
RestartDocumentEvidenceTextFull=The document was opened for editing by $actor$ in Scrive E-sign’s interface to design signing processes.
###
SMSPinDeliveredEvidenceText=$_SMSPinDeliveredEvidenceText()$
###
SMSPinSendEvidenceText=$_SMSPinSendEvidenceText()$
###
SaveSigAttachmentEvidenceText=The signatory $actor$ uploaded the attachment “$name$” requested by $author$.  The request had the following description: $description$
###
SendToPadDeviceText=The signing invitation for signatory $signatory$ was set by $actor$ to “in-person meeting”. In-person meeting enables $actor$ to present $if(full)$Scrive E-signs$else$Scrives$endif$ online interface to sign documents for $signatory$, on a device controlled by $actor$.
###
SetDocumentInviteTimeEvidenceText=The signing process was started by $actor$.
###
SignDocumentEvidenceTextSMSPinFull=The document was signed by $actor$ using PIN by SMS as identification method. $pinWasSent()$
###
SignDocumentEvidenceTextElegFull=The document was signed by $actor$ using eID as identification method. $elegVerification()$
###
SignDocumentEvidenceTextFull=The document was signed by $actor$.
###
SignDocumentEvidenceText=*
$if(sms_pin)$
  $if(full)$
    $SignDocumentEvidenceTextSMSPinFull()$
  $else$
    $if(archive)$
      $_SignDocumentEvidenceTextSimpleArchiveSMSPin()$
    $else$
      $_SignDocumentEvidenceTextSimpleNotArchiveSMSPin()$
    $endif$
  $endif$
$elseif(eleg)$
  $if(full)$
    $SignDocumentEvidenceTextElegFull()$
  $else$
    $if(archive)$
      $_SignDocumentEvidenceTextSimpleArchiveEleg()$
    $else$
      $_SignDocumentEvidenceTextSimpleNotArchiveEleg()$
    $endif$
  $endif$
$else$
  $if(full)$
    $SignDocumentEvidenceTextFull()$
  $else$
    $if(archive)$
      $_SignDocumentEvidenceTextSimpleArchiveStandard()$
    $else$
      $_SignDocumentEvidenceTextSimpleNotArchiveStandard()$
    $endif$
  $endif$
$endif$
###
SignWithELegFailureEvidenceTextFull=Scrive E-sign rejected the attempt by $signatory$ to sign the document. The reason was that the identity information of $signatory$ did not match the identity information received from the external eID system. Information given by the external eID system: $msg$.
###
SignWithELegFailureEvidenceText=*
$if(full)$
  $SignWithELegFailureEvidenceTextFull()$
$else$
  $_SignWithELegFailureEvidenceTextSimple()$
$endif$
###
SignatorySignLinkVisitedText=The document was opened by $actor$ in Scrive E-sign’s online interface to sign documents.
###
SignatoryReviewLinkVisitedText=The document was opened by $actor$  in Scrive E-sign’s online interface to sign documents.
###
SignatoryLinkVisitedText=*
$if(full)$
  $if(signing)$
    $SignatorySignLinkVisitedText()$
  $elseif(viewing)$
    $SignatoryReviewLinkVisitedText()$
  $endif$
$else$
  $if(archive)$
   $_SignatoryLinkVisitedTextArchiveSimple()$
  $else$
   $_SignatoryLinkVisitedTextNotArchiveSimple()$
  $endif$
$endif$
###
TimeoutDocumentEvidenceTextFull=The due date for signing the document passed. The possibility to sign was disabled by Scrive E-sign.
###
TimeoutDocumentEvidenceText=*
$if(full)$
  $TimeoutDocumentEvidenceTextFull()$
$else$
  $_TimeoutDocumentEvidenceTextSimple()$
$endif$
###
UpdateFieldCheckboxEvidenceText=$actor$ $if(checked)$checked$else$unchecked$endif$ the checkbox “$fieldname$”.$if(placements)$ The checkbox’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldSignatureEvidenceText=$actor$ signed in the signature box “$fieldname$”.<br/><img alt="signature" src="$value$" width="100" height="50"></img><br/>$if(placements)$The box’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldFirstNameEvidenceText=$actor$ changed the text in the text field “First name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldLastNameEvidenceText=$actor$ changed the text in the text field “Last name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCompanyEvidenceText=$actor$ changed the text in the text field “Company” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldPersonalNumberEvidenceText=$actor$ changed the text in the text field “ID number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCompanyNumberEvidenceText=$actor$ changed the text in the text field “Company number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldEmailEvidenceText=$actor$ changed the text in the text field “Email” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCustomEvidenceText=$actor$ changed the text in the text field “$fieldname$” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldMobileEvidenceText=$actor$ changed the text in the text field “Mobile number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
updateFieldPreviousValue=$if(prvblank)$blank$else$“$previousvalue$”$endif$
###
updateFieldNewValue=$if(newblank)$blank$else$“$value$”$endif$
###
updateFieldPlacements=$if(placements)$<table class="placements"><tr><td>Page</td><td>X</td><td>Y</td></tr>$placements:{placement|<tr><td>$placement.page$</td><td>$placement.x$</td><td>$placement.y$</td></tr>}$</table>$endif$
###
pinWasSent=The PIN was sent to $phone$.
###
elegVerification=
<p>The text signed in the eID client was: </p><pre>$infotext$</pre> <p>E-legitimation provider: $provider$.</p> $if(fieldsverified)$ <p>Information about signatory verified by $provider$: $if(fstnameverified)$ First name; $endif$ $if(lstnameverified)$ Last name; $endif$ $if(persnumverified)$ Personal number; $endif$</p> $endif$ <p>Signature: <div style='white-space:pre'>$signature$</div></p> $if(certificate)$ <p>Certificate: <div style='white-space:pre'>$certificate$</div></p>$endif$ $if(ocsp)$ <p>Online Certificate Status Protocol (OCSP) Response: <div style='white-space:pre'>$ocsp$</div></p>$endif$
###
invitationCustomMessageTextToSignIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToSign()$ $endif$$endif$
###
invitationCustomMessageTextToReviewIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToReview()$ $endif$$endif$
###
CancelDocumenElegEvidenceText=$_CancelDocumenElegEvidenceText()$
###
