AttachExtendedSealedFileEvidenceText=$AttachExtendedSealedFileEvidenceText()$
###
AttachGuardtimeSealedFileEvidenceText=$AttachGuardtimeSealedFileEvidenceText()$
###
AttachSealedFileEvidenceText=$AttachSealedFileEvidenceText()$
###
CancelDocumentEvidenceText=$CancelDocumentEvidenceText()$
###
ChangeSignatoryEmailWhenUndeliveredEvidenceText=The email address for the signatory was changed from $oldemail$ to $newemail$ by $actor$.
###
ChangeSignatoryPhoneWhenUndeliveredEvidenceText=The phone number for the signatory was changed from $oldphone$ to $newphone$ by $actor$.
###
CloseDocumentEvidenceText=$CloseDocumentEvidenceText()$
###
DeleteSigAttachmentEvidenceText=The attachment “$name$” previously uploaded by $actor$ on the request of $author$ was deleted by $actor$.
###
ErrorSealingDocumentEvidenceText=Due to a system error Scrive E-sign failed to finalise the document, i.e. failed to include the Verification Page, Evidence Log, Evidence of Intent, Evidence Documentation and Digital Signature Documentation into the document before sealing the Evidence Package (as defined in the Evidence Documentation) with a digital signature.
###
InvitationDeliveredByEmailText=$InvitationDeliveredByEmailText()$
###
InvitationDeliveredBySMSText=$InvitationDeliveredBySMSText()$
###
InvitationEvidenceText=$InvitationEvidenceText()$
###
InvitationUndeliveredByEmailText=$InvitationUndeliveredByEmailText()$
###
InvitationUndeliveredBySMSText=$InvitationUndeliveredBySMSText()$
###
MarkInvitationReadEvidenceText=$MarkInvitationReadEvidenceText()$
###
PreparationToPendingEvidenceText=$PreparationToPendingEvidenceText()$
###
ProlongDocumentEvidenceText=$ProlongDocumentEvidenceText()$
###
RejectDocumentEvidenceText=$RejectDocumentEvidenceText()$
###
ReminderSendText=$ReminderSendText()$
###
RemovedFromPadDeviceText=The invitation to signatory $signatory$ to sign the document by in-person meeting was withdrawn by $actor$.
###
ResealedPDFText=$ResealedPDFText()$
###
RestartDocumentEvidenceText=$RestartDocumentEvidenceText()$
###
SaveSigAttachmentEvidenceText=The signatory $actor$ uploaded the attachment “$name$” requested by $author$.  The request had the following description: $description$
###
SendToPadDeviceText=The signing invitation for signatory $signatory$ was set by $actor$ to “in-person meeting.” In-person meeting enables $actor$ to present $_TheScriveSystem()$s online interface to sign documents for $signatory$, on a device controlled by $actor$.
###
SetDocumentInviteTimeEvidenceText=The signing process was started by $actor$.
###
SignDocumentEvidenceText=$SignDocumentEvidenceText()$
###
SignatoryLinkVisitedText=$SignatoryLinkVisitedText()$
###
TimeoutDocumentEvidenceText=$TimeoutDocumentEvidenceText()$
###
UpdateFieldCheckboxEvidenceText=A checkbox was $if(checked)$checked$else$unchecked$endif$ by $actor$.$updatePlacements()$
###
UpdateFieldSignatureEvidenceText=A signature field was signed by $actor$.<br/><img alt="signature" src="$value$" width="100" height="50"></img><br/>$updatePlacements()$
###
UpdateFieldTextEvidenceText=The value of the field “$fieldname$” was changed by $actor$ to “$value$”.$updatePlacements()$
###
updatePlacements=$if(placements)$ The field had the following placements in the document:<table class="placements"><tr><td>Page</td><td>X</td><td>Y</td></tr>$placements:{placement|<tr><td>$placement.page$</td><td>$placement.x$</td><td>$placement.y$</td></tr>}$</table>$endif$
###
