####
AddSigAttachmentEvidenceText=
###
RemoveSigAttachmentsEvidenceText=
###
RemoveDocumentAttachmentEvidenceText=Attachment “$name$” deleted by $actor$.
###
AddDocumentAttachmentEvidenceText=Attachment “$name$” uploaded by $actor$.
###
PendingToAwaitingAuthorEvidenceText=$actor$ emailed notification to author ($email$) that all other parties have signed and the author is next to sign.
###
UpdateFieldsEvidenceText=
Field $fieldtype$ for signatory ($email$) was $if(checked)$checked$elseif(unchecked)$unchecked$elseif(value)$filled with "$value$"$elseif(signature)$signed <img alt="signature for $email$" src="$signature$" width="100" height="50"></img>$else$changed$endif$ by $actor$.
###
SetElegitimationIdentificationEvidenceText=$actor$ required e-legitimation verification for all signatories.
###
SetEmailIdentificationEvidenceText=$actor$ required only email verification for all signatories.
###
TimeoutDocumentEvidenceText=Signing due date passed. Possibility to sign disabled by $actor$.
###
SignDocumentEvidenceText=Signed by $actor$ $if(!eleg)$ by clicking the sign button. $else$
        using e-legitimation. The text signed in the e-legitimation client was $infotext$.
        E-legitimation provider: $provider$.
        $if(fieldsverified)$
            Information about signatory verified by $provider$:
                $if(fstnameverified)$ First name; $endif$
                $if(lstnameverified)$ Last name; $endif$
                $if(persnumverified)$ Personal number; $endif$
        $endif$
        Signature: $signature$.
        $if(certificate)$ Certificate: $certificate$ $endif$
        $if(ocsp)$ OCSP Response: $ocsp$ $endif$

    $endif$ 
###
SetInvitationDeliveryStatusEvidenceText=$actor$ reported that email invitation to sign (sent to $email$) was $status$.
###
SetDocumentUIEvidenceText=
###
SetDocumentLocaleEvidenceText=Signing process language changed to $locale$ by $actor$.
###
SetDocumentTitleEvidenceText=Title changed to “$title$” by $actor$.
###
SetDocumentAdvancedFunctionalityEvidenceText=
###
RemoveDaysToSignEvidenceText=$actor$ deactivated signing due date.
###
SetDaysToSignEvidenceText=$actor$ set due date for signing to $mdays$ days from sending the invitation.
###
SetInvitationTextEvidenceText=Invitation text changed to “$text$” by $actor$.
###
RemoveSignatoryUserEvidenceText=
###
SetSignatoryUserEvidenceText=
###
RemoveSignatoryCompanyEvidenceText=
###
SetSignatoryCompanyEvidenceText=
###
SetDocumentTagsEvidenceText=Tags set to $tags$ by $actor$.
###
SaveSigAttachmentEvidenceText=Attachment “$name$” uploaded by $actor$.
###
SaveDocumentForUserEvidenceText=
###
RestartDocumentEvidenceText=Cancelled transaction $did$ restarted by $actor$.
###
ReallyDeleteDocumentEvidenceText=
###
NewDocumentEvidenceText=New transaction “$title$” started by $actor$.
###
MarkInvitationReadEvidenceText=$actor$ reported that email invitation to sign (sent to $email$) was opened.
###
CloseDocumentEvidenceText=$actor$ closed transaction to further modifications after all signatories signed.
###
ChangeSignatoryEmailWhenUndeliveredEvidenceText=$actor$ changed the email address for signatory from $oldemail$ to $newemail$.
###
ChangeMainfileEvidenceText=$actor$ replaced “$oldfilename$” with “$newfilename$”.
###
CancelDocumenElegEvidenceText=$actor$ cancelled transaction due to mismatch of signatory information with e-legitimation data received from the e-legitimation provider. Reason: $msg$.
###
CancelDocumentEvidenceText=Transaction cancelled and invitation to sign withdrawn by $actor$.
###
AttachFileEvidenceText=$filename$ uploaded by $actor$.
###
AttachSealedFileEvidenceText=Transaction verification page, evidence log, and evidence documentation appended by $actor$. Document digitally signed and stored in database by $actor$.
###
PreparationToPendingEvidenceText=$actor$ locked document from author editing.
$if(timeouttime)$ Due date set to $timeouttime$ UTC by $actor$.$endif$
###
DeleteSigAttachmentEvidenceText=Attachment for signatory ($email$) called “$name$” deleted by $actor$ .
###
AuthorUsesCSVEvidenceText=Transaction created by $actor$ as part of a mass send out from transaction $did$.
###
ErrorDocumentEvidenceText=The following error occurred: $errmsg$. Signing disabled by $actor$.
###
MarkDocumentSeenEvidenceText=$if(ip)$ Viewed by $actor$. $else$ Marked seen for signatory ($email$) by $actor$. $endif$
###
RejectDocumentEvidenceText=$actor$ clicked the reject button.
###
SetDocumentInviteTimeEvidenceText=Signing period started by $actor$.
###
SetDocumentTimeoutTimeEvidenceText=Signing due date set to $time$ UTC by $actor$.
###
RestoreArchivedDocumentEvidenceText=
###
InvitationEvidenceText=Invitation to sign sent to $email$ by $actor$.
###
SignableFromDocumentIDWithUpdatedAuthorEvidenceText=Transaction started from template $did$ by $actor$.
###
ArchiveDocumentEvidenceText= 
###
RemoveSignatoryEvidenceText=
Signatory ($email$) removed from transaction by $actor$.
###
AddSignatoryEvidenceText=
Signatory ($email$) added to transaction by $actor$.
###
RemoveFieldEvidenceText=
Field $fieldtype$ for signatory ($email$) removed by $actor$.
###
AddFieldEvidenceText=
Field $fieldtype$ for signatory ($email$) was $if(checked)$added checked$elseif(unchecked)$added unchecked$elseif(value)$filled with "$value$"$else$added$endif$ by $actor$.$if(hasplacements)$ Field was placed onto the page at coordinates $placements:{p | (page=$p.page$, x=$p.x$, y=$p.y$) }; separator=","$.$endif$
###
ChangeFieldEvidenceText=
Field $fieldtype$ for signatory ($email$) was $if(checked)$checked$elseif(unchecked)$unchecked$elseif(value)$filled with "$value$"$elseif(signature)$signed <img alt="signature for $email$" src="$signature$" width="100" height="50"></img>$else$changed$endif$ by $actor$.$if(hasplacements)$ Field was placed onto the page at coordinates $placements:{p | (page=$p.page$, x=$p.x$, y=$p.y$) }; separator=","$.$endif$
###
ResetSignatoryDetailsEvidenceText=DO NOT USE!
###
AdminOnlySaveForUserEvidenceText=
###
SignableFromDocumentEvidenceText=Transaction started from template $did$ by $actor$.
###
TemplateFromDocumentEvidenceText= Transaction converted to template by $actor$.
###
AttachCSVUploadEvidenceText=$actor$ defined party by uploading CSV $csvtitle$.
###
SendToPadDeviceText=$actor$ sent transaction to pad device for signing by signatory ($email$).
####
RemovedFromPadDeviceText=$actor$ removed transaction from pad device for signing by signatory ($email$).
###
SetEmailAuthenticationMethodEvidenceText=$actor$ required only email authentication method for all signatories.
###
SetELegAuthenticationMethodEvidenceText=$actor$ required e-legitimation authentication method for all signatories.
###
SetEmailDeliveryMethodEvidenceText=$actor$ requested document to be available for pad signing.
###
SetPadDeliveryMethodEvidenceText=$actor$ requested document to be available only for email signing.
###
ResealedPDFText=$actor$ requested document to be resealed.
###
