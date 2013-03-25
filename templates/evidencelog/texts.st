####
AddSigAttachmentEvidenceText=
###
RemoveSigAttachmentsEvidenceText=
###
RemoveDocumentAttachmentEvidenceText=The attachment “$name$” was deleted by $actor$.
###
AddDocumentAttachmentEvidenceText=The attachment “$name$” was uploaded by $actor$.
###
PendingToAwaitingAuthorEvidenceText=$actor$ emailed a notification to the author ($email$) indicating that all other parties have signed the document and that the author is next to sign.
###
UpdateFieldsEvidenceText=
The field "$fieldtype$" for the signatory ($email$) was $if(checked)$checked$elseif(unchecked)$unchecked$elseif(value)$filled with "$value$"$elseif(signature)$signed$else$changed$endif$ by $actor$. $if(signature)$<BR/><img alt="signature for $email$" src="$signature$" width="100" height="50"></img>$endif$
###
SetElegitimationIdentificationEvidenceText=$actor$ indicated the requirement of e-legitimation verification for all signatories.
###
SetEmailIdentificationEvidenceText=$actor$ indicated the requirement of email verification only for all signatories.
###
TimeoutDocumentEvidenceText=The signing due date has passed and the possibility to sign was disabled by $actor$.
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
SetInvitationDeliveryStatusEvidenceText=$actor$ reported that the email invitation to sign (sent to $email$) was $status$.
###
SetDocumentUIEvidenceText=
###
SetDocumentLangEvidenceText=The signing process language was changed to $lang$ by $actor$.
###
SetDocumentTitleEvidenceText=The document title was changed to “$title$” by $actor$.
###
SetDocumentAdvancedFunctionalityEvidenceText=
###
RemoveDaysToSignEvidenceText=The signing due date was deactivated by $actor$.
###
SetDaysToSignEvidenceText=The signing due date was set by $actor$ to "$mdays$ days" from sending the invitation.
###
SetInvitationTextEvidenceText=The invitation text was changed to “$text$” by $actor$.
###
RemoveSignatoryUserEvidenceText=
###
SetSignatoryUserEvidenceText=
###
RemoveSignatoryCompanyEvidenceText=
###
SetSignatoryCompanyEvidenceText=
###
SetDocumentTagsEvidenceText=The document tags were set to "$tags$" by $actor$.
###
SaveSigAttachmentEvidenceText=The attachment “$name$” was uploaded by $actor$.
###
SaveDocumentForUserEvidenceText=
###
RestartDocumentEvidenceText=The cancelled transaction "$did$" was restarted by $actor$.
###
ReallyDeleteDocumentEvidenceText=
###
NewDocumentEvidenceText=The new transaction “$title$” was started by $actor$.
###
MarkInvitationReadEvidenceText=$actor$ reported that the email invitation to sign (sent to $email$) was opened.
###
CloseDocumentEvidenceText=$actor$ closed the transaction to further modifications after all signatories signed.
###
ChangeSignatoryEmailWhenUndeliveredEvidenceText=$actor$ changed the email address for the signatory from "$oldemail$" to "$newemail$".
###
ChangeMainfileEvidenceText=$actor$ replaced “$oldfilename$” with “$newfilename$”.
###
CancelDocumenElegEvidenceText=$actor$ cancelled the transaction due to the mismatch of signatory information with the e-legitimation data received from the e-legitimation provider. Reason: $msg$.
###
CancelDocumentEvidenceText=The transaction was cancelled and the invitation to sign was withdrawn by $actor$.
###
AttachFileEvidenceText=The file "$filename$" was uploaded by $actor$.
###
AttachSealedFileEvidenceText=The transaction verification page, evidence log, and evidence documentation was appended by $actor$. The document was digitally signed and stored in the database by $actor$.
###
PreparationToPendingEvidenceText=$actor$ locked the document from author editing and set the due date to "$timeouttime$ UTC".
$if(timezone)$ $actor$ provided the time zone $timezone$.$endif$
###
DeleteSigAttachmentEvidenceText=The attachment “$name$” for the signatory ($email$) was deleted by $actor$ .
###
AuthorUsesCSVEvidenceText=The transaction was created by $actor$ as part of a mass send-out from transaction "$did$".
###
ErrorDocumentEvidenceText=The following error occurred: $errmsg$. Signing was disabled by $actor$.
###
MarkDocumentSeenEvidenceText=$if(ip)$The document was viewed by $actor$. $else$The document was marked as “seen” for the signatory ($email$) by $actor$. $endif$
###
RejectDocumentEvidenceText=$actor$ clicked the reject button.
###
SetDocumentInviteTimeEvidenceText=The signing period was started by $actor$.
###
SetDocumentTimeoutTimeEvidenceText=The signing due date was set to "$time$ UTC" by $actor$.
###
RestoreArchivedDocumentEvidenceText=
###
InvitationEvidenceText=An invitation to sign was sent to $email$ by $actor$.
###
SignableFromDocumentIDWithUpdatedAuthorEvidenceText=The transaction was started from template "$did$" by $actor$.
###
ArchiveDocumentEvidenceText=
###
RemoveSignatoryEvidenceText=
The signatory ($email$) was removed from the transaction by $actor$.
###
AddSignatoryEvidenceText=
The signatory ($email$) was added to the transaction by $actor$.
###
RemoveFieldEvidenceText=
The field "$fieldtype$" for the signatory ($email$) was removed by $actor$.
###
AddFieldEvidenceText=
The field "$fieldtype$" for the signatory ($email$) was $if(checked)$added pre-checked$elseif(unchecked)$added unchecked$elseif(value)$filled with "$value$"$else$added$endif$ by $actor$.$if(hasplacements)$ This field was placed onto the document at the following coordinates: $placements:{p |(page=$p.page$, x=$p.x$, y=$p.y$)}; separator=","$.$endif$
###
ChangeFieldEvidenceText=
The field "$fieldtype$" for the signatory ($email$) was $if(checked)$checked$elseif(unchecked)$unchecked$elseif(value)$filled with "$value$"$elseif(signature)$signed$else$changed$endif$ by $actor$.$if(hasplacements)$ This field was placed onto the document at the following coordinates: $placements:{p |(page=$p.page$, x=$p.x$, y=$p.y$)}; separator=","$.$endif$ $if(signature)$ <BR/><img alt="signature for $email$" src="$signature$" width="100" height="50"></img>$endif$
###
ResetSignatoryDetailsEvidenceText=DO NOT USE!
###
AdminOnlySaveForUserEvidenceText=
###
SignableFromDocumentEvidenceText=The transaction was started from template "$did$" by $actor$.
###
TemplateFromDocumentEvidenceText=The transaction was converted to a template by $actor$.
###
AttachCSVUploadEvidenceText=$actor$ defined the party by uploading the CSV file "$csvtitle$".
###
SendToPadDeviceText=$actor$ sent the transaction for signing by signatory ($email$) to the pad device.
####
RemovedFromPadDeviceText=$actor$ removed the transaction for signing by the signatory ($email$) from the pad device.
###
SetStandardAuthenticationMethodEvidenceText=$actor$ indicated the requirement of the standard authentication method only for all signatories.
###
SetELegAuthenticationMethodEvidenceText=$actor$ indicated the requirement of the e-legitimation authentication method for all signatories.
###
SetEmailDeliveryMethodEvidenceText=$actor$ requested the document be available for pad signing.
###
SetPadDeliveryMethodEvidenceText=$actor$ requested the document be available for email signing only.
###
SetAPIDeliveryMethodEvidenceText=$actor$ requested the document be available for api signing only.
###
ResealedPDFText=$actor$ requested the document be resealed.
###
ReminderSendText= A reminder was sent. $name$’s mail delivery details were cleared.
###
SetDocumentProcessEvidenceText= The document process was set to "$process$".
###
DetachFileEvidenceText=The main file was detached by $actor$.
###
InvitationDeliveredText= The invitation was delivered.
###
InvitationUndeliveredText = The invitation was not delivered.
###
SignatoryLinkVisitedText = The link for signing the document was visited by $name$.
###
