AttachExtendedSealedFileEvidenceLog=The document was sealed with a keyless digital signature by Scrive E-sign.
###
AttachExtendedSealedFileEvidenceArchive=$_AttachExtendedSealedFileEvidenceTextSimple()$
###
AttachExtendedSealedFileEvidenceVerificationPages=$_AttachExtendedSealedFileEvidenceTextSimple()$
###
###
AttachGuardtimeSealedFileEvidenceLog=The document was sealed with a digital signature by Scrive E-sign.
###
AttachGuardtimeSealedFileEvidenceArchive=$_AttachGuardtimeSealedFileEvidenceTextSimple()$
###
AttachGuardtimeSealedFileEvidenceVerificationPages=$_AttachGuardtimeSealedFileEvidenceTextSimple()$
###
###
AttachSealedFileEvidenceLog=Scrive E-sign prepared the final PDF, including evidence collected by Scrive E-sign, before sealing with a digital signature.
###
AttachSealedFileEvidenceArchive=$_AttachSealedFileEvidenceTextSimpleArchive()$
###
AttachSealedFileEvidenceVerificationPages=$_AttachSealedFileEvidenceTextSimpleNotArchive()$
###
###
AutomaticReminderSentLogEmail=An automatic reminder to sign was sent to <span class="signatory"/> using email at the request of <span class="actor"/>.
###
AutomaticReminderSentLogSMS=An automatic reminder to sign was sent to <span class="signatory"/> using SMS at the request of <span class="actor"/>.
###
AutomaticReminderSentLogEmailSMS=An automatic reminder to sign was sent to <span class="signatory"/> using email and SMS at the request of <span class="actor"/>.
###
AutomaticReminderSentLog=*
$if(email)$
  $AutomaticReminderSentLogEmail()$
$elseif(mobile)$
  $AutomaticReminderSentLogSMS()$
$elseif(emailmobile)$
  $AutomaticReminderSentLogEmailSMS()$
$endif$
###
AutomaticReminderSentSimple=*
$if(email)$
  $_AutomaticReminderSentTextSimpleEmail()$
$elseif(mobile)$
  $_AutomaticReminderSentTextSimpleSMS()$
$elseif(emailmobile)$
  $_AutomaticReminderSentTextSimpleEmailSMS()$
$endif$
###
AutomaticReminderSentArchive=$AutomaticReminderSentSimple()$
###
AutomaticReminderSentVerificationPages=$AutomaticReminderSentSimple()$
###
###
CancelDocumentEvidenceLog=The invitation to sign was withdrawn by <span class="actor"/>. The possibility to sign was disabled by Scrive E-sign.
###
CancelDocumentEvidenceArchive=$_CancelDocumentEvidenceTextSimpleArchive()$
###
CancelDocumentEvidenceVerificationPages=$_CancelDocumentEvidenceTextSimpleNotArchive()$
###
###
ChangeSignatoryEmailWhenUndeliveredEvidenceLog=The email address for the signatory was changed from $oldemail$ to $newemail$ by <span class="actor"/>.
###
ChangeSignatoryPhoneWhenUndeliveredEvidenceLog=The phone number for the signatory was changed from $oldphone$ to $newphone$ by <span class="actor"/>.
###
ChangeAuthenticationMethodStandardToELegEvidenceLog=<span class="actor"/> changed authentication for <span class="signatory"/> from “no extra authentication” to “eID”.
###
ChangeAuthenticationMethodStandardToSMSEvidenceLog=<span class="actor"/> changed authentication for <span class="signatory"/> from “no extra authentication” to “PIN by SMS”.
###
ChangeAuthenticationMethodELegToStandardEvidenceLog=<span class="actor"/> changed authentication for <span class="signatory"/> from “eID” to “no extra authentication”.
###
ChangeAuthenticationMethodELegToSMSEvidenceLog=<span class="actor"/> changed authentication for <span class="signatory"/> from “eID” to “PIN by SMS”.
###
ChangeAuthenticationMethodSMSToStandardEvidenceLog=<span class="actor"/> changed authentication for <span class="signatory"/> from “PIN by SMS” to “no extra authentication”.
###
ChangeAuthenticationMethodSMSToElegEvidenceLog=<span class="actor"/> changed authentication for <span class="signatory"/> from “PIN by SMS” to “eID”.
###
###
CloseDocumentEvidenceLog=Scrive E-sign locked the document from further modifications by the signing parties.
###
CloseDocumentEvidenceArchive=$_CloseDocumentEvidenceTextSimple()$
###
CloseDocumentEvidenceVerificationPages=$_CloseDocumentEvidenceTextSimple()$

###
DeleteSigAttachmentEvidenceLog=The attachment “$name$” previously uploaded by <span class="actor"/> on the request of <span class="author"/> was deleted by <span class="actor"/>.
###
ErrorSealingDocumentEvidenceLog=Due to a system error Scrive E-sign failed to finalise the document, i.e. failed to include the Verification Page, Evidence Log, Evidence of Intent, Evidence Documentation and Digital Signature Documentation into the document before sealing the Evidence Package (as defined in the Evidence Documentation) with a digital signature.
###
ReviewInvitationDeliveredByEmailLog=Scrive E-sign’s external email delivery system reported that the invitation to review was delivered to <span class="signatory"/> by email.
###
SigningInvitationDeliveredByEmailLog=Scrive E-sign’s external email delivery system reported that the invitation to sign was delivered to <span class="signatory"/> by email.
###
###
InvitationDeliveredByEmailLog=*
$if(signing)$
  $SigningInvitationDeliveredByEmailLog()$
$elseif(viewing)$
  $ReviewInvitationDeliveredByEmailLog()$
$endif$
###
InvitationDeliveredByEmailSimple=*
$if(signing)$
  $_InvitationDeliveredByEmailTextToSignSimple()$
$elseif(viewing)$
  $_InvitationDeliveredByEmailTextToReviewSimple()$
$endif$
###
InvitationDeliveredByEmailArchive=$InvitationDeliveredByEmailSimple()$
###
InvitationDeliveredByEmailVerificationPages=$InvitationDeliveredByEmailSimple()$
###
###
ReviewInvitationDeliveredBySMSLog=Scrive E-sign’s external SMS delivery system reported that the invitation to review was delivered to <span class="signatory"/> by SMS.
###
SigningInvitationDeliveredBySMSLog=Scrive E-sign’s external SMS delivery system reported that the invitation to sign was delivered to <span class="signatory"/> by SMS.
###
###
InvitationDeliveredBySMSLog=*
$if(signing)$
  $SigningInvitationDeliveredBySMSLog()$
$elseif(viewing)$
  $ReviewInvitationDeliveredBySMSLog()$
$endif$
###
InvitationDeliveredBySMSSimple=*
$if(signing)$
  $_InvitationDeliveredBySMSTextToSignSimple()$
$elseif(viewing)$
  $_InvitationDeliveredBySMSTextToReviewSimple()$
$endif$
###
InvitationDeliveredBySMSArchive=$InvitationDeliveredBySMSSimple()$
###
InvitationDeliveredBySMSVerificationPages=$InvitationDeliveredBySMSSimple()$
###
###
SigningInvitationEvidenceLogEmail=An invitation to sign was sent to <span class="signatory"/> using email. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToSignIfThereIsOne()$
###
SigningInvitationEvidenceLogSMS=An invitation to sign was sent to <span class="signatory"/> using SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToSignIfThereIsOne()$
###
SigningInvitationEvidenceLogEmailSMS=An invitation to sign was sent to <span class="signatory"/> using email and SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToSignIfThereIsOne()$
###
ReviewInvitationEvidenceLogEmail=An invitation to review was sent to <span class="signatory"/> using email. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToReviewIfThereIsOne()$
###
ReviewInvitationEvidenceLogSMS=An invitation to review was sent to <span class="signatory"/> using SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToReviewIfThereIsOne()$
###
ReviewInvitationEvidenceLogEmailSMS=An invitation to review was sent to <span class="signatory"/> using email and SMS. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $invitationCustomMessageTextToReviewIfThereIsOne()$
###
InvitationEvidenceLog=*
$if(signing)$
  $if(email)$
      $SigningInvitationEvidenceLogEmail()$
  $elseif(mobile)$
      $SigningInvitationEvidenceLogSMS()$
  $elseif(emailmobile)$
      $SigningInvitationEvidenceLogEmailSMS()$
  $endif$
$elseif(viewing)$
  $if(email)$
      $ReviewInvitationEvidenceLogEmail()$
  $elseif(mobile)$
      $ReviewInvitationEvidenceLogSMS()$
  $elseif(emailmobile)$
      $ReviewInvitationEvidenceLogEmailSMS()$
  $endif$
$endif$
###
InvitationEvidenceArchive=*
$if(signing)$
  $if(email)$
        $_InvitationEvidenceTextToSignArchiveSimpleEmail()$
  $elseif(mobile)$
        $_InvitationEvidenceTextToSignArchiveSimpleSMS()$
  $elseif(emailmobile)$
        $_InvitationEvidenceTextToSignArchiveSimpleEmailSMS()$
  $endif$
$elseif(viewing)$
  $if(email)$
        $_InvitationEvidenceTextToReviewArchiveSimpleEmail()$
  $elseif(mobile)$
        $_InvitationEvidenceTextToReviewArchiveSimpleSMS()$
  $elseif(emailmobile)$
        $_InvitationEvidenceTextToReviewArchiveSimpleEmailSMS()$
  $endif$
$endif$
###
InvitationEvidenceVerificationPages=*
$if(signing)$
  $if(email)$
        $_InvitationEvidenceTextToSignNotArchiveSimpleEmail()$
  $elseif(mobile)$
        $_InvitationEvidenceTextToSignNotArchiveSimpleSMS()$
  $elseif(emailmobile)$
        $_InvitationEvidenceTextToSignNotArchiveSimpleEmailSMS()$
  $endif$
$elseif(viewing)$
  $if(email)$
        $_InvitationEvidenceTextToReviewNotArchiveSimpleEmail()$
  $elseif(mobile)$
        $_InvitationEvidenceTextToReviewNotArchiveSimpleSMS()$
  $elseif(emailmobile)$
        $_InvitationEvidenceTextToReviewNotArchiveSimpleEmailSMS()$
  $endif$
$endif$
###
###
InvitationUndeliveredByEmailLog=Scrive E-sign’s external email delivery system reported that the invitation to <span class="signatory"/> could not be delivered by email and continued attempts were cancelled.
###
InvitationUndeliveredByEmailArchive=$_InvitationUndeliveredByEmailTextSimpleArchive()$
###
InvitationUndeliveredByEmailVerificationPages=$_InvitationUndeliveredByEmailTextSimpleNotArchive()$
###
###
InvitationUndeliveredBySMSLog=Scrive E-sign’s external SMS delivery system reported that the invitation to <span class="signatory"/> could not be delivered by SMS and continued attempts were cancelled.
###
InvitationUndeliveredBySMSArchive=$_InvitationUndeliveredBySMSTextSimpleArchive()$
###
InvitationUndeliveredBySMSVerificationPages=$_InvitationUndeliveredBySMSTextSimpleNotArchive()$
###
MarkSigningInvitationReadEvidenceLog=Scrive E-sign’s external email delivery system reported that the invitation to sign (sent to $signatory_email$) was opened.
###
MarkReviewInvitationReadEvidenceLog=Scrive E-sign’s external email delivery system reported that the invitation to review (sent to $signatory_email$) was opened.
###
MarkInvitationReadEvidenceLog=*
$if(signing)$
  $MarkSigningInvitationReadEvidenceLog()$
$elseif(viewing)$
  $MarkReviewInvitationReadEvidenceLog()$
$endif$
###
MarkInvitationReadEvidenceSimple=*
$if(signing)$
  $_MarkInvitationReadEvidenceTextToSignSimple()$
$elseif(viewing)$
  $_MarkInvitationReadEvidenceTextToReviewSimple()$
$endif$
###
MarkInvitationReadEvidenceArchive=$MarkInvitationReadEvidenceSimple()$
###
MarkInvitationReadEvidenceVerificationPages=$MarkInvitationReadEvidenceSimple()$
###
###
PreparationToPendingEvidenceLog=The preparation of the signing process settings was finished by <span class="actor"/>, and Scrive E-sign locked the document from editing. The signing process language was set to $lang$, and the signing due date was set to $timeouttime$ UTC.$if(timezone)$ The time zone $timezone$ was provided by <span class="actor"/>$endif$. The signing process was initiated.
###
PreparationToPendingEvidenceArchive=$_PreparationToPendingEvidenceTextSimpleArchive()$
###
PreparationToPendingEvidenceVerificationPages=$_PreparationToPendingEvidenceTextSimpleNotArchive()$
###
ProlongDocumentEvidenceLog=The due date for signing was prolonged by <span class="actor"/>.
###
ProlongDocumentEvidenceArchive=$_ProlongDocumentEvidenceTextArchive()$
###
ProlongDocumentEvidenceVerificationPages=$_ProlongDocumentEvidenceTextNotArchive()$
###
###
RejectDocumentEvidenceLog=The invitation to sign was rejected by <span class="actor"/>. The possibility to sign was disabled by Scrive E-sign.$if(text)$ The rejection message from <span class="actor"/> was: <BR/> <i>$text:noescape()$</i>$endif$
###
RejectDocumentEvidenceArchive=$_RejectDocumentEvidenceTextSimpleArchive()$
###
RejectDocumentEvidenceVerificationPages=$_RejectDocumentEvidenceTextSimpleNotArchive()$
###
###
ReminderSendLog=*
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
###
ReminderSendArchive=*
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
###
ReminderSendVerificationPages=$ReminderSendLog()$
###
###
RemovedFromPadDeviceLog=The invitation to signatory <span class="signatory"/> to sign the document by in-person meeting was withdrawn by <span class="actor"/>.
###
ResealedPDFLog=Preparation of the Evidence Package was requested by <span class="actor"/>, a person authorised to administer Scrive E-sign.
###
RestartDocumentEvidenceLog=The document was opened for editing by <span class="actor"/> in Scrive E-sign’s interface to design signing processes.
###
RestartDocumentEvidenceArchive=$_RestartDocumentEvidenceTextSimpleArchive()$
###
RestartDocumentEvidenceVerificationPages=$_RestartDocumentEvidenceTextSimpleNotArchive()$
###
###
SMSPinDeliveredEvidenceLog=$_SMSPinDeliveredEvidenceText()$
###
SMSPinDeliveredEvidenceArchive=$_SMSPinDeliveredEvidenceText()$
###
SMSPinDeliveredEvidenceVerificationPages=$_SMSPinDeliveredEvidenceText()$
###
###
SMSPinSendEvidenceLog=$_SMSPinSendEvidenceText()$
###
SMSPinSendEvidenceArchive=$_SMSPinSendEvidenceText()$
###
SMSPinSendEvidenceVerificationPages=$_SMSPinSendEvidenceText()$
###
###
SaveSigAttachmentEvidenceLog=<span class="actor"/> uploaded the attachment “$name$” requested by <span class="author"/>.  The request had the following description: $description$
###
SendToPadDeviceLog=The signing invitation for signatory <span class="signatory"/> was set by <span class="actor"/> to “in-person meeting”. In-person meeting enables <span class="actor"/> to present $if(full)$Scrive E-signs$else$Scrives$endif$ online interface to sign documents for <span class="signatory"/>, on a device controlled by <span class="actor"/>.
###
SetDocumentInviteTimeEvidenceLog=The signing process was started by <span class="actor"/>.
###
###
SignDocumentEvidenceLogSMSPin=The document was signed by <span class="actor"/> using PIN by SMS as identification method. $pinWasSent()$
###
SignDocumentEvidenceLogEleg=The document was signed by <span class="actor"/> using eID as identification method. $elegVerification()$
###
SignDocumentEvidenceLogStandard=The document was signed by <span class="actor"/>.
###
SignDocumentEvidenceLog=*
$if(sms_pin)$
    $SignDocumentEvidenceLogSMSPin()$
$elseif(eleg)$
    $SignDocumentEvidenceLogEleg()$
$else$
    $SignDocumentEvidenceLogStandard()$
$endif$
###
SignDocumentEvidenceArchive=*
$if(sms_pin)$
      $_SignDocumentEvidenceTextSimpleArchiveSMSPin()$
$elseif(eleg)$
      $_SignDocumentEvidenceTextSimpleArchiveEleg()$
$else$
      $_SignDocumentEvidenceTextSimpleArchiveStandard()$
$endif$
###
SignDocumentEvidenceVerificationPages=*
$if(sms_pin)$
      $_SignDocumentEvidenceTextSimpleNotArchiveSMSPin()$
$elseif(eleg)$
      $if(eid_signatory_name)$
        $_SignDocumentEvidenceTextSimpleNotArchiveElegWithName()$
      $else$
        $_SignDocumentEvidenceTextSimpleNotArchiveEleg()$
      $endif$
$else$
      $_SignDocumentEvidenceTextSimpleNotArchiveStandard()$
$endif$
###
SignWithELegFailureEvidenceArchive=$_SignWithELegFailureEvidenceTextSimple()$
###
SignWithELegFailureEvidenceVerificationPages=$_SignWithELegFailureEvidenceTextSimple()$
###
SignWithELegFailureEvidenceLog=Scrive E-sign rejected the attempt by <span class="signatory"/> to sign the document. The reason was that the identity information of <span class="signatory"/> did not match the identity information received from the external eID system. Information given by the external eID system: $msg$.
###
SignatorySignLinkVisitedLog=The document was opened by <span class="actor"/> in Scrive E-sign’s online interface to sign documents.
###
SignatoryReviewLinkVisitedLog=The document was opened by <span class="actor"/>  in Scrive E-sign’s online interface to sign documents.
###
SignatoryLinkVisitedLog=*
$if(signing)$
  $SignatorySignLinkVisitedLog()$
$elseif(viewing)$
  $SignatoryReviewLinkVisitedLog()$
$endif$
###
SignatoryLinkVisitedArchive=$_SignatoryLinkVisitedTextArchiveSimple()$
###
SignatoryLinkVisitedVerificationPages=$_SignatoryLinkVisitedTextNotArchiveSimple()$
###
###
TimeoutDocumentEvidenceLog=The due date for signing the document passed. The possibility to sign was disabled by Scrive E-sign.
###
TimeoutDocumentEvidenceArchive=$_TimeoutDocumentEvidenceTextSimple()$
###
TimeoutDocumentEvidenceVerificationPages=$_TimeoutDocumentEvidenceTextSimple()$
###
###
UpdateFieldCheckboxEvidenceLog=<span class="actor"/> $if(checked)$checked$else$unchecked$endif$ the checkbox “$fieldname$”.$if(placements)$ The checkbox’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldSignatureEvidenceLog=<span class="actor"/> signed in the signature box “$fieldname$”.<br/><img alt="signature" src="$value$" width="100" height="50"></img><br/>$if(placements)$The box’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldFirstNameEvidenceLog=<span class="actor"/> changed the text in the text field “First name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldLastNameEvidenceLog=<span class="actor"/> changed the text in the text field “Last name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCompanyEvidenceLog=<span class="actor"/> changed the text in the text field “Company” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldPersonalNumberEvidenceLog=<span class="actor"/> changed the text in the text field “ID number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCompanyNumberEvidenceLog=<span class="actor"/> changed the text in the text field “Company number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldEmailEvidenceLog=<span class="actor"/> changed the text in the text field “Email” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCustomEvidenceLog=<span class="actor"/> changed the text in the text field “$fieldname$” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldMobileEvidenceLog=<span class="actor"/> changed the text in the text field “Mobile number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldNameEvidenceLog=<span class="actor"/> changed the text in the text field “Name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
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
<p>The text signed in the eID client was:</p>
<pre style='white-space:normal'>$signed_text$</pre>
<p>eID provider: $provider$</p>
<p>Data returned from the eID:
  <br/>Name: $signatory_name$
  <br/>ID number: $signatory_personal_number$
</p>
<p>Signature: <div style='white-space:pre'>$signature$</div></p>
<p>Online Certificate Status Protocol (OCSP) Response: <div style='white-space:pre'>$ocsp_response$</div></p>
###
invitationCustomMessageTextToSignIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToSign()$ $endif$$endif$
###
invitationCustomMessageTextToReviewIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToReview()$ $endif$$endif$
###
