####
AddSigAttachmentEvidenceText=
###
RemoveSigAttachmentsEvidenceText=
###
RemoveDocumentAttachmentEvidenceText=File with ID $fid$ removed from document by $actor$ .
###
AddDocumentAttachmentEvidenceText=File with ID $fid$ attached to Document by $actor$.
###
PendingToAwaitingAuthorEvidenceText=
###
UpdateFieldsEvidenceText=Information for signatory with email $email$ for field $fieldtype$ was set to $value$ by $actor$.
###
SetElegitimationIdentificationEvidenceText=Document identification type set to $identification$ by $actor$.
###
SetEmailIdentificationEvidenceText=Document identification type set to $identification$ by $actor$.
###
TimeoutDocumentEvidenceText=Document timed out by $actor$.
###
SignDocumentEvidenceText=Document signed by $actor$ $if(!eleg)$ . $else$
        using e-legitimation. The signed text was $infotext$.
        Provider: $provider$.
        $if(fieldsverified)$
            Fields verified:
                $if(fstnameverified)$ First name; $endif$
                $if(lstnameverified)$ Last name; $endif$
                $if(persnumverified)$ Personal number; $endif$
        $endif$
        Signature: $signature$.
        $if(certificate)$ Certificate: $certificate$ $endif$
        $if(ocsp)$ OCSP Response: $ocsp$ $endif$

    $endif$ 
###
SetInvitationDeliveryStatusEvidenceText=Delivery status for signatory with id $sid$ set to $status$ by $actor$.
###
SetDocumentUIEvidenceText=$if(footer)$ Document mail footer set to $footer$ by $actor$. $else$ Document mail footer removed by $actor$. $endif$
###
SetDocumentLocaleEvidenceText=Document locale set to $locale$ by $actor$.
###
SetDocumentTitleEvidenceText=Document title set to $title$ by actor.
###
SetDocumentAdvancedFunctionalityEvidenceText=
###
RemoveDaysToSignEvidenceText= Days to sign set to $mdays$ by $actor$.
###
SetDaysToSignEvidenceText= Days to sign set to $mdays$ by $actor$.
###
SetInvitationTextEvidenceText=Invitation text set to $text$ by $actor$.
###
RemoveSignatoryUserEvidenceText=
###
SetSignatoryUserEvidenceText=
###
RemoveSignatoryCompanyEvidenceText=
###
SetSignatoryCompanyEvidenceText=
###
 SetDocumentTagsEvidenceText=Document tags set to $tags$ by $actor$.
###
SaveSigAttachmentEvidenceText=Saving attachment with name $name$ for signatory by $actor$.
###
SaveDocumentForUserEvidenceText=Saving document to user account with email $email$ by $actor$.
###
RestartDocumentEvidenceText=Document restarted from document with id $did$  by $actor$.
###
ReallyDeleteDocumentEvidenceText= The document was removed from the rubbish bin for $if(company)$ the company with admin email $email$ $else$ the user with email $email$ $endif$ by $actor$.
###
NewDocumentEvidenceText=Document $title$ created by $actor$.
###
MarkInvitationReadEvidenceText=Invitation sent to $email$ was opened, as reported by $actor$.
###
CloseDocumentEvidenceText=The document was closed by $actor$ after all signatories had signed.
###
ChangeSignatoryEmailWhenUndeliveredEvidenceText=Changed the email address for signatory from $oldemail$ to $newemail$ by $actor$.
###
ChangeMainfileEvidenceText=Main file was changed by $actor$.
###
CancelDocumenElegEvidenceText=The document was canceled due to a mismatch with e-legitimation data by $actor$. Reason: $msg$.
###
CancelDocumentEvidenceText=The document was canceled by $actor$.
###
AttachFileEvidenceText=Uploaded main PDF by $actor$.
###
AttachSealedFileEvidenceText=Sealed file attached by $actor$.
###
PreparationToPendingEvidenceText=Document was put into Pending state by $actor$. $if(timeouttime)$ Timeout time set to $timeouttime$ UTC.$endif$
###
DeleteSigAttachmentEvidenceText= Signatory attachment for signatory was deleted by $actor$.
###
AuthorUsesCSVEvidenceText=Document created from CSV file by $actor$.
###
ErrorDocumentEvidenceText=The following error occured on the document: $errmsg$
###
MarkDocumentSeenEvidenceText=$if(ip)$ Document viewed by $actor$. $else$ Marking document seen for signatory with id $sid$ by $actor$. $endif$
###
RejectDocumentEvidenceText=Document rejected for signatory with email $email$ by $actor$.
###
SetDocumentInviteTimeEvidenceText=Document invite time set to $time$ UTC by $actor$.
###
SetDocumentTimeoutTimeEvidenceText= Document timeout time set to $time$ UTC by $actor$ .
###
RestoreArchivedDocumentEvidenceText=Document restored from the rubbish bin for $if(company)$ the company with admin email $email$ $else$ the user with email $email$ $endif$ by $actor$.
###
InvitationEvidenceText=Invitation sent to $email$ by $actor$.
###
SignableFromDocumentIDWithUpdatedAuthorEvidenceText= Document created from template with id $did$ by $actor$ .
###
ArchiveDocumentEvidenceText= Moved document to rubbish bin for $if(company)$ company with admin email $email$ $else$ user with email $email$ $endif$ by $actor$.
###
ResetSignatoryDetailsEvidenceText=
    $if(added)$
        $if(signatory)$
          Signatory with email $email$ added by $actor$.
        $elseif(field)$
          Field $fieldtype$ for signatory with email $email$ set to $value$ with placements $placements$ by $actor$.
        $endif$
    $elseif(changed)$
        $if(field)$
          Field $fieldtype$ for signatory with email $email$ set to $value$ with placements $palcements$ by $actor$.
        $endif$
    $elseif(removed)$
        $if(signatory)$
          Signatory with email $email$ removed by $actor$.
        $elseif(field)$
          Field $fieldtype$ for signatory with email $email$ removed by $actor$.
        $endif$
    $endif$
###
AdminOnlySaveForUserEvidenceText=Document saved for user with email $email$ by $actor$.
###
SignableFromDocumentEvidenceText=Document created from template by $actor$.
###
TemplateFromDocumentEvidenceText= Document converted to template by $actor$.
###
AttachCSVUploadEvidenceText=Attached CSV $csvtitle$ to document by $actor$.
###
SendToPadDeviceText=Document sent to pad device for signatory $sid$ by $actor$.
####
RemovedFromPadDeviceText=Document removed from pad device for signatory $sid$ by $actor$.
###
