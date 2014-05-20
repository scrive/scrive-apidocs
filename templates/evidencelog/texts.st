AttachExtendedSealedFileEvidenceText=*
$if(full)$
  The document was sealed with a keyless digital signature by Scrive E-sign.
$else$
  $_AttachExtendedSealedFileEvidenceTextSimple()$
$endif$
###
AttachGuardtimeSealedFileEvidenceText=*
$if(full)$
  The document was sealed with a digital signature by Scrive E-sign.
$else$
  $_AttachGuardtimeSealedFileEvidenceTextSimple()$
$endif$
###
AttachSealedFileEvidenceText=*
$if(full)$
  Scrive E-sign prepared the final PDF, including evidence collected by Scrive E-sign, before sealing with a digital signature.
$else$
  $if(archive)$
    $_AttachSealedFileEvidenceTextSimpleArchive()$
  $else$
    $_AttachSealedFileEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
AutomaticReminderSentText=*
$if(full)$
  An automatic reminder to sign was sent to $signatory$ using $_delivery_method()$ at the request of $author$.
$else$
  $_AutomaticReminderSentTextSimple()$
$endif$
###
CancelDocumentEvidenceText=*
$if(full)$
  The invitation to sign was withdrawn by $actor$. The possibility to sign was disabled by Scrive E-sign.
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
CloseDocumentEvidenceText=
$if(full)$
  Scrive E-sign locked the document from further modifications by the signing parties.
$else$
  $_CloseDocumentEvidenceTextSimple()$
$endif$
###
DeleteSigAttachmentEvidenceText=The attachment “$name$” previously uploaded by $actor$ on the request of $author$ was deleted by $actor$.
###
ErrorSealingDocumentEvidenceText=Due to a system error Scrive E-sign failed to finalise the document, i.e. failed to include the Verification Page, Evidence Log, Evidence of Intent, Evidence Documentation and Digital Signature Documentation into the document before sealing the Evidence Package (as defined in the Evidence Documentation) with a digital signature.
###
InvitationDeliveredByEmailText=*
$if(full)$
  $if(signing)$
    Scrive E-sign’s external email delivery system reported that the invitation to sign was delivered to $signatory$ by email.
  $elseif(viewing)$
    Scrive E-sign’s external email delivery system reported that the invitation to review was delivered to $signatory$ by email.
  $endif$
$else$
  $if(signing)$
    $_InvitationDeliveredByEmailTextToSignSimple()$
  $elseif(viewing)$
    $_InvitationDeliveredByEmailTextToReviewSimple()$
  $endif$
$endif$
###
InvitationDeliveredBySMSText=*
$if(full)$
  $if(signing)$
    Scrive E-sign’s external SMS delivery system reported that the invitation to sign was delivered to $signatory$ by SMS.
  $elseif(viewing)$
  Scrive E-sign’s external SMS delivery system reported that the invitation to review was delivered to $signatory$ by SMS.
  $endif$
$else$
  $if(signing)$
    $_InvitationDeliveredBySMSTextToSignSimple()$
  $elseif(viewing)$
    $_InvitationDeliveredBySMSTextToReviewSimple()$
  $endif$
$endif$
###
InvitationEvidenceText=*
$if(signing)$
  $if(full)$
    An invitation to sign was sent to $signatory$ using $_delivery_method()$. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $_invitationCustomMessageTextToSign()$
  $else$
    $if(archive)$
      $_InvitationEvidenceTextToSignArchiveSimple()$
    $else$
      $_InvitationEvidenceTextToSignNotArchiveSimple()$
    $endif$
  $endif$
$elseif(viewing)$
  $if(full)$
    An invitation to review was sent to $signatory$ using $_delivery_method()$. The invitation included a link that when clicked lead to Scrive E-sign’s online interface to sign documents. $_invitationCustomMessageTextToReview()$
  $else$
    $if(archive)$
      $_InvitationEvidenceTextToReviewArchiveSimple()$
    $else$
      $_InvitationEvidenceTextToReviewNotArchiveSimple()$
    $endif$
  $endif$
$endif$
###
InvitationUndeliveredByEmailText=*
$if(full)$
  Scrive E-sign’s external email delivery system reported that the invitation to $signatory$ could not be delivered by email and continued attempts were cancelled.
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
  Scrive E-sign’s external SMS delivery system reported that the invitation could not be delivered by SMS and continued attempts were cancelled.
$else$
  $if(archive)$
    $_InvitationUndeliveredBySMSTextSimpleArchive()$
  $else$
    $_InvitationUndeliveredBySMSTextSimpleNotArchive()$
  $endif$
$endif$
###
MarkInvitationReadEvidenceText=*
$if(full)$
  $if(signing)$
  Scrive E-sign’s external email delivery system reported that the invitation to sign (sent to $signatory_email$) was opened.
  $elseif(viewing)$
    Scrive E-sign’s external email delivery system reported that the invitation to review (sent to $signatory_email$) was opened.
  $endif$
$else$
  $if(signing)$
    $_MarkInvitationReadEvidenceTextToSignSimple()$
  $elseif(viewing)$
    $_MarkInvitationReadEvidenceTextToReviewSimple()$
  $endif$
$endif$
###
PreparationToPendingEvidenceText=*
$if(full)$
  The preparation of the signing process settings was finished by $actor$, and Scrive E-sign locked the document from editing. The signing process language was set to $lang$, and the signing due date was set to $timeouttime$ UTC.$if(timezone)$ The time zone $timezone$ was provided by $actor$$endif$. The signing process was initiated.
$else$
  $if(archive)$
    $_PreparationToPendingEvidenceTextSimpleArchive()$
  $else$
    $_PreparationToPendingEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
###
ProlongDocumentEvidenceText=*
$if(archive)$
  $_ProlongDocumentEvidenceTextArchive()$
$else$
  The due date for signing was prolonged by $actor$.
$endif$
###
RejectDocumentEvidenceText=*
$if(full)$
  The invitation to sign was rejected by $actor$. The possibility to sign was disabled by Scrive E-sign.$if(text)$ The rejection message from $actor$ was: <BR/> <i>$text:noescape()$</i>$endif$
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
    $_ReminderSendTextToSignArchive()$
  $elseif(viewing)$
    $_ReminderSendTextToReviewArchive()$
  $endif$
$else$
  $if(signing)$
    $_ReminderSendTextToSignNotArchive()$
  $elseif(viewing)$
    $_ReminderSendTextToReviewNotArchive()$
  $endif$
$endif$
###
RemovedFromPadDeviceText=The invitation to signatory $signatory$ to sign the document by in-person meeting was withdrawn by $actor$.
###
ResealedPDFText=Preparation of the Evidence Package was requested by $actor$, a person authorised to administer Scrive E-sign.
###
RestartDocumentEvidenceText=*
$if(full)$
  The document was opened for editing by $actor$ in Scrive E-sign’s interface to design signing processes
$else$
  $if(archive)$
    $_RestartDocumentEvidenceTextSimpleArchive()$
  $else$
    $_RestartDocumentEvidenceTextSimpleNotArchive()$
  $endif$
$endif$
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
SignDocumentEvidenceText=*
$if(sms_pin)$
  $if(full)$
    The document was signed by $actor$$_using_identification_method()$. $_pinWasSend()$
  $else$
    $if(archive)$
      $_SignDocumentEvidenceTextSimpleArchiveSMSPin()$
    $else$
      $_SignDocumentEvidenceTextSimpleNotArchiveSMSPin()$
    $endif$
  $endif$
$elseif(eleg)$
  $if(full)$
    The document was signed by $actor$$_using_identification_method()$. $_elegVerified()$
  $else$
    $if(archive)$
      $_SignDocumentEvidenceTextSimpleArchiveEleg()$
    $else$
      $_SignDocumentEvidenceTextSimpleNotArchiveEleg()$
    $endif$
  $endif$
$else$
  $if(full)$
    The document was signed$_using_identification_method()$.
  $else$
    $if(archive)$
      $_SignDocumentEvidenceTextSimpleArchiveStandard()$
    $else$
      $_SignDocumentEvidenceTextSimpleNotArchiveStandard()$
    $endif$
  $endif$
$endif$
###
SignWithELegFailureEvidenceText=*
$if(full)$
  Scrive E-sign rejected the attempt by $signatory$ to sign the document. The reason was that the identity information of $signatory$ did not match the identity information received from the external eID system. Information given by the external eID system: $msg$.
$else$
  $_SignWithELegFailureEvidenceTextSimple()$
$endif$
###
SignatoryLinkVisitedText=*
$if(full)$
  $if(signing)$
    The link in the invitation to sign was clicked by $actor$. The document was opened by $actor$ in Scrive E-sign’s online interface to sign documents.
  $elseif(viewing)$
    The link in the invitation to review was clicked by $actor$. The document was opened by $actor$  in Scrive E-sign’s online interface to sign documents.
  $endif$
$else$
  $if(archive)$
   $_SignatoryLinkVisitedTextArchiveSimple()$
  $else$
   $_SignatoryLinkVisitedTextNotArchiveSimple()$
  $endif$
$endif$
###
TimeoutDocumentEvidenceText=*
$if(full)$
  The due date for signing the document passed. The possibility to sign was disabled by Scrive E-sign.
$else$
  $_TimeoutDocumentEvidenceTextSimple()$
$endif$
###
UpdateFieldCheckboxEvidenceText=A checkbox was $if(checked)$checked$else$unchecked$endif$ by $actor$.$updatePlacements()$
###
UpdateFieldSignatureEvidenceText=A signature field was signed by $actor$.<br/><img alt="signature" src="$value$" width="100" height="50"></img><br/>$updatePlacements()$
###
UpdateFieldTextEvidenceText=The value of the field “$fieldname$” was changed by $actor$ to “$value$”.$updatePlacements()$
###
updatePlacements=$if(placements)$ The field had the following placements in the document:<table class="placements"><tr><td>Page</td><td>X</td><td>Y</td></tr>$placements:{placement|<tr><td>$placement.page$</td><td>$placement.x$</td><td>$placement.y$</td></tr>}$</table>$endif$
###
