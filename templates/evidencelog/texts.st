####
AddSigAttachmentEvidenceText=
###
RemoveSigAttachmentsEvidenceText=
###
RemoveDocumentAttachmentEvidenceText=
###
AddDocumentAttachmentEvidenceText=
###
PendingToAwaitingAuthorEvidenceText=Scrive emailed a notification to the author of the signing process ($email$) that all other parties have signed and the author is next to sign.
###
UpdateFieldsEvidenceText=
###
SetElegitimationIdentificationEvidenceText=
###
SetEmailIdentificationEvidenceText=
###
TimeoutDocumentEvidenceText=The due date for signing the document passed. The possibility to sign was disabled by Scrive.
###
SignDocumentEvidenceText=The document was signed by $actor$ $if(!eleg)$ by clicking the sign button.$else$
        using e-legitimation.
        <p>The text signed in the e-legitimation client was: </p>
        <pre>$infotext$</pre>
        <p>E-legitimation provider: $provider$.</p>
        $if(fieldsverified)$
            <p>Information about signatory verified by $provider$:
                $if(fstnameverified)$ First name; $endif$
                $if(lstnameverified)$ Last name; $endif$
                $if(persnumverified)$ Personal number; $endif$</p>
        $endif$
        <p>Signature: <div style="white-space:pre">$signature$</div></p>
        $if(certificate)$ <p>Certificate: <div style="white-space:pre">$certificate$</div></p>$endif$
        $if(ocsp)$ <p>Online Certificate Status Protocol (OCSP) Response: <div style="white-space:pre">$ocsp$</div></p>$endif$
    $endif$
###
SetInvitationDeliveryStatusEvidenceText=Scrive´s external email delivery system reported that the email invitation to sign (sent to $email$) was $status$.
###
SetDocumentUIEvidenceText=
###
SetDocumentLangEvidenceText=
###
SetDocumentTitleEvidenceText=
###
SetDocumentAdvancedFunctionalityEvidenceText=
###
RemoveDaysToSignEvidenceText=
###
SetDaysToSignEvidenceText=
###
SetInvitationTextEvidenceText=
###
RemoveSignatoryUserEvidenceText=
###
SetSignatoryUserEvidenceText=
###
RemoveSignatoryCompanyEvidenceText=
###
SetSignatoryCompanyEvidenceText=
###
SetDocumentTagsEvidenceText=
###
SaveSigAttachmentEvidenceText=The attachment with the name “$name$” was uploaded by $actor$.
###
SaveDocumentForUserEvidenceText=
###
RestartDocumentEvidenceText=The cancelled signing process was restarted by $actor$.
###
ProlongDocumentEvidenceText=The due date for signing was prolonged by $actor$.
###
ReallyDeleteDocumentEvidenceText=
###
NewDocumentEvidenceText=
###
MarkInvitationReadEvidenceText=Scrive´s external email delivery system reported that the email invitation to sign (sent to $email$) was opened.
###
CloseDocumentEvidenceText=After all signatories signed, Scrive closed the signing process to further modifications.
###
ChangeSignatoryEmailWhenUndeliveredEvidenceText=The email address for signatory was changed from $oldemail$ to $newemail$ by $actor$.
###
ChangeMainfileEvidenceText=
###
ChangeSignatoryPhoneWhenUndeliveredEvidenceText=$actor$ changed the phone for the signatory from "$oldphone$" to "$newphone$".
###
CancelDocumenElegEvidenceText=Scrive cancelled the signing process due to mismatch of signatory information with the eID data received from the eID provider. Reason provided by the eID provider: $msg$.
###
CancelDocumentEvidenceText=The signing process was cancelled by $actor$.
###
AttachFileEvidenceText=
###
AttachSealedFileEvidenceText=The verification page, evidence log, evidence of intent, evidence documentation and digital signature documentation were appended to the document by Scrive (before digitally signing the document with a Guardtime digital signature).
###
PreparationToPendingEvidenceText=The editing of the signing process was finalised by $actor$. Scrive locked the signing process from editing, the language was set to $lang$, and the signing due date was set to $timeouttime$ UTC.
$if(timezone)$ The time zone $timezone$ was provided by $actor$.$endif$
###
DeleteSigAttachmentEvidenceText=The attachment with the name “$name$” was deleted by $actor$.
###
AuthorUsesCSVEvidenceText=The signing process was started by Scrive using the mass signing feature.
###
ErrorDocumentEvidenceText=The following error occurred: $errmsg$. The signing process was cancelled by Scrive.
###
MarkDocumentSeenEvidenceText=
###
RejectDocumentEvidenceText=The reject button was clicked by $actor$ and the signing process was cancelled by Scrive.
###
SetDocumentInviteTimeEvidenceText=The signing process was started by $actor$.
###
SetDocumentTimeoutTimeEvidenceText=
###
RestoreArchivedDocumentEvidenceText=
###
InvitationEvidenceText=The invitation to sign was sent to $email$ by Scrive. The invitation includes a clickable link that when clicked leads to the online signing page powered by Scrive.
###
SignableFromDocumentIDWithUpdatedAuthorEvidenceText=
###
ArchiveDocumentEvidenceText=
###
RemoveSignatoryEvidenceText=
###
AddSignatoryEvidenceText=
###
RemoveFieldEvidenceText=
###
AddFieldEvidenceText=
###
ChangeFieldEvidenceText=
###
ResetSignatoryDetailsEvidenceText=
###
AdminOnlySaveForUserEvidenceText=
###
SignableFromDocumentEvidenceText=
###
TemplateFromDocumentEvidenceText=
###
AttachCSVUploadEvidenceText=
###
SendToPadDeviceText=The signing invitation for signatory ($email$) was set to "in person signing". In person signing does not prompt any signing invitation to be sent.
####
RemovedFromPadDeviceText=In person signing for signatory ($email$) was withdrawn by $actor$.
###
SetStandardAuthenticationMethodEvidenceText=No additional authentication method for signatories to sign was required by $actor$.
###
SetELegAuthenticationMethodEvidenceText=eID authentication method for signatories to sign was required by $actor$.
###
SetEmailDeliveryMethodEvidenceText=
###
SetPadDeliveryMethodEvidenceText=
###
SetAPIDeliveryMethodEvidenceText=
###
ResealedPDFText=The document was resealed by $actor$. Resealing is a manual request by an admin in the Scrive backend to seal the document. Scrive sealing of the document includes appending the verification page, evidence log, evidence of intent, evidence documentation, digital signature documentation and the Guardtime digital signature. Resealing is done in cases when a system error prevented the sealing to be executed by the Scrive system.
###
ReminderSendText=A reminder to sign was sent by the Scrive system. The Scrive system reset the delivery status details (sent/received/read invitation/read online).
###
SetDocumentProcessEvidenceText=
###
DetachFileEvidenceText=
###
InvitationDeliveredText=Scrive´s external email delivery system reported that the email invitation to sign was delivered.
###
InvitationUndeliveredText=Scrive´s external email delivery system reported that the email invitation to sign could not be delivered.
###
SignatoryLinkVisitedText=The page for signing the document was visited by $name$.
###
ProlongDocumentEvidenceText=Document $did$ prolonged by $actor$
####
