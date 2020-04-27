AttachExtendedSealedFileEvidenceLog=Scrive eSign sealed the Evidence Package (as defined in the Service Description) with a keyless digital signature.
###
AttachExtendedSealedFileEvidenceArchive=$_AttachExtendedSealedFileEvidenceTextSimple()$
###
###
AttachDigitalSignatureSealedFileEvidenceLog=Scrive eSign sealed the final PDF with a digital signature.
###
AttachDigitalSignatureSealedFileEvidenceArchive=$_AttachGuardtimeSealedFileEvidenceTextSimple()$
###
###
AttachSealedFileEvidenceLog=Scrive eSign prepared the final PDF, including evidence collected by Scrive eSign, before sealing with a digital signature.
###
AttachSealedFileEvidenceArchive=$_AttachSealedFileEvidenceTextSimpleArchive()$
###
###
AutomaticReminderSentLogEmail=Scrive eSign sent an automatic reminder to sign using email to <span class="signatory"/> at the request of <span class="actor"/>.
###
AutomaticReminderSentLogSMS=Scrive eSign sent an automatic reminder to sign using SMS to <span class="signatory"/> at the request of <span class="actor"/>.
###
AutomaticReminderSentLogEmailSMS=Scrive eSign sent an automatic reminder to sign using email and SMS to <span class="signatory"/> at the request of <span class="actor"/>.
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
###
CancelDocumentEvidenceLog=The user <span class="actor"/> withdrew the invitation to sign and Scrive eSign disabled the possibility to sign for all parties.
###
CancelDocumentEvidenceArchive=$_CancelDocumentEvidenceTextSimpleArchive()$
###
###
###
ChangeSignatoryEmailEvidenceLog=The user <span class="actor"/> changed the email address for the party from $oldemail$ to $newemail$.
###
ChangeSignatoryEmailWhenUndeliveredEvidenceLog=The user <span class="actor"/> changed the email address for the party from $oldemail$ to $newemail$.
###
ChangeSignatoryPhoneEvidenceLog=The user <span class="actor"/> changed the mobile number for the party from $oldphone$ to $newphone$.
###
ChangeSignatoryPhoneWhenUndeliveredEvidenceLog=The user <span class="actor"/> changed the mobile number for the party from $oldphone$ to $newphone$.
###
ChangeAuthenticationToSignFrom=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from $auth$
###
ChangeAuthenticationToSignTo=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> to $auth$
###
ChangeAuthenticationToViewFrom=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from $auth$
###
ChangeAuthenticationToViewTo=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> to $auth$
###
ChangeAuthenticationToViewArchivedFrom=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from $auth$
###
ChangeAuthenticationToViewArchivedTo=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> to $auth$
###
ChangeAuthenticationToSignFromStandardLog=$ChangeAuthenticationToSignFrom(auth='“no extra authentication”')$
###
ChangeAuthenticationToSignFromSMSPinLog=$ChangeAuthenticationToSignFrom(auth='“PIN by SMS”')$
###
ChangeAuthenticationToSignFromSEBankIDLog=$ChangeAuthenticationToSignFrom(auth='“Swedish BankID”')$
###
ChangeAuthenticationToSignFromNOBankIDLog=$ChangeAuthenticationToSignFrom(auth='“Norwegian BankID”')$
###
ChangeAuthenticationToSignFromDKNemIDLog=$ChangeAuthenticationToSignFrom(auth='“Danish NemID”')$
###
ChangeAuthenticationToSignFromIDINLog=$ChangeAuthenticationToSignFrom(auth='“Dutch iDIN”')$
###
ChangeAuthenticationToSignFromFITupasLog=$ChangeAuthenticationToSignFrom(auth='“Finnish FTN')$
###
ChangeAuthenticationToSignFromOnfidoDocumentCheckLog=$ChangeAuthenticationToSignFrom(auth='“Onfido (document check only)”')$
###
ChangeAuthenticationToSignFromOnfidoDocumentAndPhotoCheckLog=$ChangeAuthenticationToSignFrom(auth='“Onfido (document check + facial comparison)”')$
###
ChangeAuthenticationToSignToStandardLog=$ChangeAuthenticationToSignTo(auth='“no extra authentication”')$
###
ChangeAuthenticationToSignToSMSPinLog=$ChangeAuthenticationToSignTo(auth='“PIN by SMS”')$
###
ChangeAuthenticationToSignToSEBankIDLog=$ChangeAuthenticationToSignTo(auth='“Swedish BankID”')$
###
ChangeAuthenticationToSignToNOBankIDLog=$ChangeAuthenticationToSignTo(auth='“Norwegian BankID”')$
###
ChangeAuthenticationToSignToDKNemIDLog=$ChangeAuthenticationToSignTo(auth='“Danish NemID”')$
###
ChangeAuthenticationToSignToIDINLog=$ChangeAuthenticationToSignTo(auth='“Dutch iDIN”')$
###
ChangeAuthenticationToSignToFITupasLog=$ChangeAuthenticationToSignTo(auth='“Finnish FTN')$
###
ChangeAuthenticationToSignToOnfidoDocumentCheckLog=$ChangeAuthenticationToSignTo(auth='“Onfido (document check only)”')$
###
ChangeAuthenticationToSignToOnfidoDocumentAndPhotoCheckLog=$ChangeAuthenticationToSignTo(auth='“Onfido (document check + facial comparison)”')$
###
ChangeAuthenticationToViewFromStandardLog=$ChangeAuthenticationToViewFrom(auth='“no extra authentication”')$
###
ChangeAuthenticationToViewFromSMSPinLog=$ChangeAuthenticationToViewFrom(auth='“PIN by SMS”')$
###
ChangeAuthenticationToViewFromSEBankIDLog=$ChangeAuthenticationToViewFrom(auth='“Swedish BankID”')$
###
ChangeAuthenticationToViewFromNOBankIDLog=$ChangeAuthenticationToViewFrom(auth='“Norwegian BankID”')$
###
ChangeAuthenticationToViewFromDKNemIDLog=$ChangeAuthenticationToViewFrom(auth='“Danish NemID”')$
###
ChangeAuthenticationToViewFromFITupasLog=$ChangeAuthenticationToViewFrom(auth='“Finnish TUPAS”')$
###
ChangeAuthenticationToViewFromVerimiLog=$ChangeAuthenticationToViewFrom(auth='“Verimi”')$
###
ChangeAuthenticationToViewFromIDINLog=$ChangeAuthenticationToViewFrom(auth='“iDIN“')$
###
ChangeAuthenticationToViewToStandardLog=$ChangeAuthenticationToViewTo(auth='“no extra authentication”')$
###
ChangeAuthenticationToViewToSMSPinLog=$ChangeAuthenticationToViewTo(auth='“PIN by SMS”')$
###
ChangeAuthenticationToViewToSEBankIDLog=$ChangeAuthenticationToViewTo(auth='“Swedish BankID”')$
###
ChangeAuthenticationToViewToNOBankIDLog=$ChangeAuthenticationToViewTo(auth='“Norwegian BankID”')$
###
ChangeAuthenticationToViewToDKNemIDLog=$ChangeAuthenticationToViewTo(auth='“Danish NemID”')$
###
ChangeAuthenticationToViewToFITupasLog=$ChangeAuthenticationToViewTo(auth='“Finnish TUPAS”')$
###
ChangeAuthenticationToViewToVerimiLog=$ChangeAuthenticationToViewTo(auth='“Verimi”')$
###
ChangeAuthenticationToViewToIDINLog=$ChangeAuthenticationToViewTo(auth='“iDIN“')$
###
ChangeAuthenticationToViewArchivedFromStandardLog=$ChangeAuthenticationToViewArchivedFrom(auth='“no extra authentication”')$
###
ChangeAuthenticationToViewArchivedFromSMSPinLog=$ChangeAuthenticationToViewArchivedFrom(auth='“PIN by SMS”')$
###
ChangeAuthenticationToViewArchivedFromSEBankIDLog=$ChangeAuthenticationToViewArchivedFrom(auth='“Swedish BankID”')$
###
ChangeAuthenticationToViewArchivedFromNOBankIDLog=$ChangeAuthenticationToViewArchivedFrom(auth='“Norwegian BankID”')$
###
ChangeAuthenticationToViewArchivedFromDKNemIDLog=$ChangeAuthenticationToViewArchivedFrom(auth='“Danish NemID”')$
###
ChangeAuthenticationToViewArchivedFromFITupasLog=$ChangeAuthenticationToViewArchivedFrom(auth='“Finnish TUPAS”')$
###
ChangeAuthenticationToViewArchivedFromVerimiLog=$ChangeAuthenticationToViewArchivedFrom(auth='“Verimi”')$
###
ChangeAuthenticationToViewArchivedFromIDINLog=$ChangeAuthenticationToViewArchivedFrom(auth='“iDIN“')$
###
ChangeAuthenticationToViewArchivedToStandardLog=$ChangeAuthenticationToViewArchivedTo(auth='“no extra authentication”')$
###
ChangeAuthenticationToViewArchivedToSMSPinLog=$ChangeAuthenticationToViewArchivedTo(auth='“PIN by SMS”')$
###
ChangeAuthenticationToViewArchivedToSEBankIDLog=$ChangeAuthenticationToViewArchivedTo(auth='“Swedish BankID”')$
###
ChangeAuthenticationToViewArchivedToNOBankIDLog=$ChangeAuthenticationToViewArchivedTo(auth='“Norwegian BankID”')$
###
ChangeAuthenticationToViewArchivedToDKNemIDLog=$ChangeAuthenticationToViewArchivedTo(auth='“Danish NemID”')$
###
ChangeAuthenticationToViewArchivedToFITupasLog=$ChangeAuthenticationToViewArchivedTo(auth='“Finnish TUPAS”')$
###
ChangeAuthenticationToViewArchivedToVerimiLog=$ChangeAuthenticationToViewArchivedTo(auth='“Verimi”')$
###
ChangeAuthenticationToViewArchivedToIDINLog=$ChangeAuthenticationToViewArchivedTo(auth='“iDIN“')$
###
###
###
CloseDocumentEvidenceLog=Scrive eSign locked the document from further modifications by the parties.
###
CloseDocumentEvidenceArchive=$_CloseDocumentEvidenceTextSimple()$
###
CustomEventEvidenceLog=Event generated by initiator:
$text$
###
DeleteSigAttachmentEvidenceLog=The party <span class="actor"/> deleted the attachment “$name$” previously uploaded by the party <span class="actor"/>.
###
ErrorSealingDocumentEvidenceLog=Due to a system error Scrive eSign failed to finalise the Evidence Package (as defined in the Service Description) before sealing with a digital signature.
###
ApproveInvitationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation to approve the document sent via email to <span class="signatory"/> was delivered.
###
ReviewInvitationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation to review the document sent via email to <span class="signatory"/> was delivered.
###
SigningInvitationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation to sign the document sent via email to <span class="signatory"/> was delivered.
###
###
InvitationDeliveredByEmailLog=*
$if(signing)$
  $SigningInvitationDeliveredByEmailLog()$
$elseif(approving)$
  $ApproveInvitationDeliveredByEmailLog()$
$elseif(viewing)$
  $ReviewInvitationDeliveredByEmailLog()$
$endif$
###
InvitationDeliveredByEmailSimple=*
$if(signing)$
  $_InvitationDeliveredByEmailTextToSignSimple()$
$elseif(approving)$
  $_InvitationDeliveredByEmailTextToApproveSimple()$
$elseif(viewing)$
  $_InvitationDeliveredByEmailTextToReviewSimple()$
$endif$
###
InvitationDeliveredByEmailArchive=$InvitationDeliveredByEmailSimple()$
###
###
ApproveInvitationDeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation to approve the document sent via SMS to <span class="signatory"/> was delivered.
###
ReviewInvitationDeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation to review the document sent via SMS to <span class="signatory"/> was delivered.
###
SigningInvitationDeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation to sign the document sent via SMS to <span class="signatory"/> was delivered.
###
###
InvitationDeliveredBySMSLog=*
$if(signing)$
  $SigningInvitationDeliveredBySMSLog()$
$elseif(approving)$
  $ApproveInvitationDeliveredBySMSLog()$
$elseif(viewing)$
  $ReviewInvitationDeliveredBySMSLog()$
$endif$
###
InvitationDeliveredBySMSSimple=*
$if(signing)$
  $_InvitationDeliveredBySMSTextToSignSimple()$
$elseif(approving)$
  $_InvitationDeliveredBySMSTextToApproveSimple()$
$elseif(viewing)$
  $_InvitationDeliveredBySMSTextToReviewSimple()$
$endif$
###
InvitationDeliveredBySMSArchive=$InvitationDeliveredBySMSSimple()$
###
###
SigningInvitationEvidenceLogEmail=Scrive eSign sent an invitation to sign the document via email ($used_email_address$) to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to sign documents. $invitationCustomMessageTextIfThereIsOne()$
###
SigningInvitationEvidenceLogSMS=Scrive eSign sent an invitation to sign the document via SMS ($used_mobile_number$) to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to sign documents. $invitationCustomMessageTextIfThereIsOne()$
###
SigningInvitationEvidenceLogEmailSMS=Scrive eSign sent an invitation to sign the document via email ($used_email_address$) and SMS ($used_mobile_number$) to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to sign documents. $invitationCustomMessageTextIfThereIsOne()$
###
ApproveInvitationEvidenceLogEmail=Scrive eSign sent an invitation to approve the document via email to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to approve documents. $invitationCustomMessageTextIfThereIsOne()$
###
ApproveInvitationEvidenceLogSMS=Scrive eSign sent an invitation to approve the document via SMS to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to approve documents. $invitationCustomMessageTextIfThereIsOne()$
###
ApproveInvitationEvidenceLogEmailSMS=Scrive eSign sent an invitation to approve the document via email and SMS to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to approve documents. $invitationCustomMessageTextIfThereIsOne()$
###
ReviewInvitationEvidenceLogEmail=Scrive eSign sent an invitation to review the document via email to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to review documents. $invitationCustomMessageTextIfThereIsOne()$
###
ReviewInvitationEvidenceLogSMS=Scrive eSign sent an invitation to review the document via SMS to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to review documents. $invitationCustomMessageTextIfThereIsOne()$
###
ReviewInvitationEvidenceLogEmailSMS=Scrive eSign sent an invitation to review the document via email and SMS to <span class="signatory"/>. The invitation included a link that when clicked led to Scrive eSign’s online interface to review documents. $invitationCustomMessageTextIfThereIsOne()$
###
InvitationEvidenceLog=*
$if(signing)$
  $if(emailmobile)$
      $SigningInvitationEvidenceLogEmailSMS()$
  $elseif(mobile)$
      $SigningInvitationEvidenceLogSMS()$
  $elseif(email)$
      $SigningInvitationEvidenceLogEmail()$
  $endif$
$elseif(approving)$
  $if(emailmobile)$
      $ApproveInvitationEvidenceLogEmailSMS()$
  $elseif(mobile)$
      $ApproveInvitationEvidenceLogSMS()$
  $elseif(email)$
      $ApproveInvitationEvidenceLogEmail()$
  $endif$
$elseif(viewing)$
  $if(emailmobile)$
      $ReviewInvitationEvidenceLogEmailSMS()$
  $elseif(mobile)$
      $ReviewInvitationEvidenceLogSMS()$
  $elseif(email)$
      $ReviewInvitationEvidenceLogEmail()$
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
$elseif(approving)$
  $if(email)$
        $_InvitationEvidenceTextToApproveArchiveSimpleEmail()$
  $elseif(mobile)$
        $_InvitationEvidenceTextToApproveArchiveSimpleSMS()$
  $elseif(emailmobile)$
        $_InvitationEvidenceTextToApproveArchiveSimpleEmailSMS()$
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
InvitationUndeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation sent via email to <span class="signatory"/> could not be delivered and continued attempts were cancelled.
###
InvitationUndeliveredByEmailArchive=$_InvitationUndeliveredByEmailTextSimpleArchive()$
###
InvitationUndeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation sent via SMS to <span class="signatory"/> could not be delivered and continued attempts were cancelled.
###
InvitationUndeliveredBySMSArchive=$_InvitationUndeliveredBySMSTextSimpleArchive()$
###
MarkInvitationReadEvidenceLog=Scrive eSign’s external email delivery system reported that the invitation sent to $signatory_email$ was opened.
###
MarkInvitationReadEvidenceSimple=*
$if(signing)$
  $_MarkInvitationReadEvidenceTextToSignSimple()$
$elseif(approving)$
  $_MarkInvitationReadEvidenceTextToApproveSimple()$
$elseif(viewing)$
  $_MarkInvitationReadEvidenceTextToReviewSimple()$
$endif$
###
MarkInvitationReadEvidenceArchive=$MarkInvitationReadEvidenceSimple()$
###
MarkInvitationReadEvidenceVerificationPages=$MarkInvitationReadEvidenceSimple()$
###
###
PreparationToPendingEvidenceLog=The initiator <span class="actor"/> initiated the signing workflow and Scrive eSign a) locked the signing workflow from further editing, b) set the signing workflow language to $lang$, c) set the signing due date to $timeouttime$ UTC,$if(timezone)$ d) set the time zone to $timezone$$endif$ and e) initiated the signing workflow.
###
PreparationToPendingEvidenceArchive=$_PreparationToPendingEvidenceTextSimpleArchive()$
###
ProlongDocumentEvidenceLog=The user <span class="actor"/> prolonged the signing due date.
###
ProlongDocumentEvidenceArchive=$_ProlongDocumentEvidenceTextArchive()$
###
###
RejectDocumentEvidenceLog=The signatory <span class="actor"/> rejected the invitation to sign and Scrive eSign disabled the possibility to sign for all parties.$if(text)$ The rejection message from the signatory <span class="actor"/> was: <BR/> <i>$text$</i>$endif$
###
RejectDocumentEvidenceArchive=*
$if(text)$
  $_RejectDocumentEvidenceTextSimpleArchiveWithText()$
$else$
  $_RejectDocumentEvidenceTextSimpleArchive()$
$endif$
###
RejectDocumentByApproverEvidenceLog=The signatory <span class="actor"/> rejected the invitation to approve and Scrive eSign disabled the possibility to sign for all parties.$if(text)$ The rejection message from the signatory <span class="actor"/> was: <BR/> <i>$text$</i>$endif$
###
RejectDocumentByApproverEvidenceArchive=*
$if(text)$
  $_RejectDocumentByApproverEvidenceTextSimpleArchiveWithText()$
$else$
  $_RejectDocumentByApproverEvidenceTextSimpleArchive()$
$endif$
###
ReminderSendLog=*
$if(signing)$
  $if(email)$
    $if(text)$
      $_SignReminderSendEmailLogWithText()$
    $else$
      $_SignReminderSendEmailLog()$
    $endif$
  $elseif(mobile)$
    $if(text)$
      $_SignReminderSendSMSLogWithText()$
    $else$
      $_SignReminderSendSMSLog()$
    $endif$
  $elseif(emailmobile)$
    $if(text)$
      $_SignReminderSendEmailSMSLogWithText()$
    $else$
      $_SignReminderSendEmailSMSLog()$
    $endif$
  $endif$
$elseif(viewing)$
  $if(email)$
    $if(text)$
      $_ReviewReminderSendEmailLogWithText()$
    $else$
      $_ReviewReminderSendEmailLog()$
    $endif$
  $elseif(mobile)$
    $if(text)$
      $_ReviewReminderSendSMSLogWithText()$
    $else$
      $_ReviewReminderSendSMSLog()$
    $endif$
  $elseif(emailmobile)$
    $if(text)$
      $_ReviewReminderSendEmailSMSLogWithText()$
    $else$
      $_ReviewReminderSendEmailSMSLog()$
    $endif$
  $endif$
$elseif(approving)$
  $if(email)$
    $if(text)$
      $_ApproveReminderSendEmailLogWithText()$
    $else$
      $_ApproveReminderSendEmailLog()$
    $endif$
  $elseif(mobile)$
    $if(text)$
      $_ApproveReminderSendSMSLogWithText()$
    $else$
      $_ApproveReminderSendSMSLog()$
    $endif$
  $elseif(emailmobile)$
    $if(text)$
      $_ApproveReminderSendEmailSMSLogWithText()$
    $else$
      $_ApproveReminderSendEmailSMSLog()$
    $endif$
  $endif$
$endif$
###
ReminderSendArchive=*
$if(signing)$
  $if(email)$
      $if(text)$
        $_ReminderSendTextToSignArchiveEmailWithText()$
      $else$
        $_ReminderSendTextToSignArchiveEmail()$
      $endif$
  $elseif(mobile)$
      $if(text)$
        $_ReminderSendTextToSignArchiveSMSWithText()$
      $else$
        $_ReminderSendTextToSignArchiveSMS()$
      $endif$
  $elseif(emailmobile)$
      $if(text)$
        $_ReminderSendTextToSignArchiveEmailSMSWithText()$
      $else$
        $_ReminderSendTextToSignArchiveEmailSMS()$
      $endif$
  $endif$
$elseif(viewing)$
  $if(email)$
      $if(text)$
        $_ReminderSendTextToReviewArchiveEmailWithText()$
      $else$
        $_ReminderSendTextToReviewArchiveEmail()$
      $endif$
  $elseif(mobile)$
      $if(text)$
        $_ReminderSendTextToReviewArchiveSMSWithText()$
      $else$
        $_ReminderSendTextToReviewArchiveSMS()$
      $endif$
  $elseif(emailmobile)$
      $if(text)$
        $_ReminderSendTextToReviewArchiveEmailSMSWithText()$
      $else$
        $_ReminderSendTextToReviewArchiveEmailSMS()$
      $endif$
  $endif$
$elseif(approving)$
  $if(email)$
      $if(text)$
        $_ReminderSendTextToApproveArchiveEmailWithText()$
      $else$
        $_ReminderSendTextToApproveArchiveEmail()$
      $endif$
  $elseif(mobile)$
      $if(text)$
        $_ReminderSendTextToApproveArchiveSMSWithText()$
      $else$
        $_ReminderSendTextToApproveArchiveSMS()$
      $endif$
  $elseif(emailmobile)$
      $if(text)$
        $_ReminderSendTextToApproveArchiveEmailSMSWithText()$
      $else$
        $_ReminderSendTextToApproveArchiveEmailSMS()$
      $endif$
  $endif$$endif$
###
###
RemovedFromPadDeviceLog=The invitation to signatory <span class="signatory"/> to sign the document by in-person meeting was withdrawn by <span class="actor"/>.
###
ResealedPDFLog=Scrive eSign’s administrator <span class="actor"/>, a person authorised to administer Scrive eSign, requested preparation of the Evidence Package.
###
RestartDocumentEvidenceLog=The user <span class="actor"/> edited the signing workflow in Scrive eSign’s interface to design signing workflows.
###
RestartDocumentEvidenceArchive=$_RestartDocumentEvidenceTextSimpleArchive()$
###
###
SMSPinDeliveredEvidenceLog=$_SMSPinDeliveredEvidenceText()$
###
SMSPinDeliveredEvidenceArchive=$_SMSPinDeliveredEvidenceText()$
###
###
SMSPinSendEvidenceLog=$_SMSPinSendEvidenceText()$
###
SMSPinSendEvidenceArchive=$_SMSPinSendEvidenceText()$
###
###
SaveSigAttachmentEvidenceLog=The signatory <span class="actor"/> uploaded the attachment “$name$” requested by the workflow author. The request had the following description: <BR/> <i>$description$</i>
###
SendToPadDeviceLog=The signing invitation for signatory <span class="signatory"/> was set by <span class="actor"/> to “in-person meeting”. In-person meeting enables <span class="actor"/> to present $if(full)$Scrive E-signs$else$Scrives$endif$ online interface to sign documents for <span class="signatory"/>, on a device controlled by <span class="actor"/>.
###
SetDocumentInviteTimeEvidenceLog=The signing process was started by <span class="actor"/>.
###
###
SignDocumentEvidenceLogSMSPin=The signatory <span class="actor"/> signed the document using PIN by SMS as authentication method. $pinWasSent()$
###
SignDocumentEvidenceLogEleg=The document was signed by <span class="actor"/> using $providerType()$ through $provider()$ as the authentication method. $elegVerificationSign()$
###
SignDocumentEvidenceLogStandard=The signatory <span class="actor"/> signed the document.
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
$elseif(provider_nobankid)$
      $_SignDocumentEvidenceTextSimpleArchiveNOBankID()$
$elseif(provider_dknemid)$
      $_SignDocumentEvidenceTextSimpleArchiveDKNemID()$
$elseif(eleg)$
      $_SignDocumentEvidenceTextSimpleArchiveEleg()$
$else$
      $_SignDocumentEvidenceTextSimpleArchiveStandard()$
$endif$
###
SignatorySignLinkVisitedLog=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to sign documents.
###
SignatoryApproveLinkVisitedLog=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to approve documents.
###
SignatoryReviewLinkVisitedLog=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to review documents.
###
SignatoryLinkVisitedLog=*
$if(signing)$
  $SignatorySignLinkVisitedLog()$
$elseif(approving)$
  $SignatoryApproveLinkVisitedLog()$
$elseif(viewing)$
  $SignatoryReviewLinkVisitedLog()$
$endif$
###
SignatoryLinkVisitedArchive=$_SignatoryLinkVisitedTextArchiveSimple()$
###
###
TimeoutDocumentEvidenceLog=The due date for signing the document passed. Scrive eSign disabled the possibility to sign for all parties.
###
TimeoutDocumentEvidenceArchive=$_TimeoutDocumentEvidenceTextSimple()$
###
AuthorAttachmentAcceptedLog=The signatory <span class="actor"/> checked the checkbox "$attachment_name$". The text next to the checkbox was "$attachment_acceptance_text$".
###
SignatoryAttachmentNotUploadedLog=The signatory <span class="actor"/> checked the checkbox "$attachment_name$". The text next to the checkbox was "$attachment_nothing_to_upload_text$".
###
UpdateFieldCheckboxEvidenceLog=The signatory <span class="actor"/> $if(checked)$checked$else$unchecked$endif$ the checkbox “$fieldname$”.$if(placements)$ The checkbox’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldRadioGroupEvidenceLog=The signatory <span class="actor"/> selected radio button "$value$" in radio group "$fieldname$". Possible options were: $fieldvalues$. $if(placements)$ The radio buttons placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldSignatureEvidenceLog=The signatory <span class="actor"/> signed in the signature box “$fieldname$”.<br/><img alt="signature" src="$value$" width="100" height="50"></img><br/>$if(placements)$The box’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldFirstNameEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “First name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldLastNameEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “Last name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCompanyEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “Company” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldPersonalNumberEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “ID number” from $personalNumber(value=updateFieldPreviousValue())$ to $personalNumber(value=updateFieldNewValue())$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCompanyNumberEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “Company number” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldEmailEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “Email” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldCustomEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “$fieldname$” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldMobileEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “Mobile” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
UpdateFieldNameEvidenceLog=The signatory <span class="actor"/> changed the text in the text field “Name” from $updateFieldPreviousValue()$ to $updateFieldNewValue()$.$if(placements)$ The field’s placement in the Main Document:$updateFieldPlacements()$$endif$
###
updateFieldPreviousValue=$if(prvblank)$blank$else$“$previousvalue$”$endif$
###
updateFieldNewValue=$if(newblank)$blank$else$“$value$”$endif$
###
updateFieldPlacements=$if(placements)$<table class="placements"><tr><td>Page</td><td>X</td><td>Y</td></tr>$placements:{placement|<tr><td>$placement.page$</td><td>$placement.x$</td><td>$placement.y$</td></tr>}$</table>$endif$
###
pinWasSent=The PIN was sent to $phone$.
###
personalNumber=
$if(hide_pn)$
  <i>*Redacted by author*</i>
$else$
  $value$
$endif$
###
elegVerificationSign=
<br/>
$if(signed_text)$
<br/>
<p>The text signed in the $providerType$ client was:</p>
<pre style='white-space:normal'>$signed_text$</pre>
$endif$
<br/>
$if(provider_sebankid)$
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  <br/>ID number: $personalNumber(value=signatory_personal_number)$
  <br/>IP: $signatory_ip$
</p>
$endif$
$if(provider_nobankid)$
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  <br/>PID: $signatory_pid$
</p>
$endif$
$if(provider_dknemid)$
<p>Signing transaction with $provider()$ was restricted to ID number: $if(signatory_personal_number_from_signlink)$$personalNumber(value=signatory_personal_number_from_signlink)$$else$$personalNumber(value=signatory_personal_number)$$endif$</p>
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  $if(signatory_personal_number)$<br/>ID number: $personalNumber(value=signatory_personal_number)$$endif$
  $if(signatory_ip)$<br/>IP: $signatory_ip$$endif$
</p>
$endif$
$if(provider_idin)$
<p>Authentication at the point of signing with $provider()$.</p>
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  $if(signatory_dob)$<br/>Date of birth: $signatory_dob$$endif$
  $if(signatory_customer_id)$<br/>Customer ID: $signatory_customer_id$$endif$
</p>
$endif$
$if(provider_fitupas)$
<p>Authentication at the point of signing with $provider()$.</p>
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  $if(signatory_dob)$<br/>Date of birth: $signatory_dob$$endif$
  $if(signatory_personal_number)$<br/>Personal identification number: $personalNumber(value=signatory_personal_number)$$endif$
</p>
$endif$
$if(provider_onfido)$
<p>Authentication at the point of signing with $provider()$.</p>
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  <br/>Date of birth: $signatory_dob$
</p>
$endif$
$if(signature)$
<br/>
<p>Signature: <div style="word-break:break-word;">$personalNumber(value=signature)$</div></p>
$if(provider_sebankid)$
<p>Online Certificate Status Protocol (OCSP) Response:
  <div style="word-break:break-word;">$ocsp_response$</div>
</p>
$endif$
$endif$
###
invitationCustomMessageTextIfThereIsOne=
$if(text)$$if(!mobile)$The invitation contained the following message: <BR/> <i>$text$</i>$endif$$endif$
###
invitationCustomMessageTextToSignIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToSign()$ $endif$$endif$
###
invitationCustomMessageTextToReviewIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToReview()$ $endif$$endif$
###
invitationCustomMessageTextToApproveIfThereIsOne=
$if(text)$$if(!mobile)$ $_invitationCustomMessageTextToApprove()$ $endif$$endif$
###
CancelDocumenElegEvidenceText=$_CancelDocumenElegEvidenceText()$
###
###
###
VisitedViewForSigningEvidenceArchive=$_VisitedViewForSigningEvidenceArchive()$
###
###
VisitedViewForSigningEvidenceLogForSign=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to sign documents.
###
VisitedViewForSigningEvidenceLogForApproval=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to approve documents.
###
VisitedViewForSigningEvidenceLogForReview=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to review documents.
###
VisitedViewForSigningEvidenceLog=*
$if(signing)$
  $VisitedViewForSigningEvidenceLogForSign()$
$elseif(approving)$
  $VisitedViewForSigningEvidenceLogForApproval()$
$elseif(viewing)$
  $VisitedViewForSigningEvidenceLogForReview()$
$endif$
###
VisitedViewForAuthenticationEvidenceLog=The party <span class="actor"/> opened the view to verify their identity before viewing the document.
###
VisitedViewForAuthenticationEvidenceArchive=$_VisitedViewForAuthenticationEvidenceArchive()$
###
AuthenticatedToViewEvidenceLog=$if(provider_sms_pin)$ $AuthenticatedToViewEvidenceLogWithSMSPin()$ $else$ $AuthenticatedToViewEvidenceLogWithEID()$ $endif$
###
AuthenticatedToViewEvidenceLogWithSMSPin=<span class="actor"/> verified their identity through “PIN by SMS” to access the document.
###
AuthenticatedToViewEvidenceLogWithEID=<span class="actor"/> verified their $providerType()$ identity through $provider()$ to access the document. $elegVerificationAuth()$
###
elegVerificationAuth=
<br/>
<br/>
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  $if(signatory_personal_number)$
  <br/>ID number: $personalNumber(value=signatory_personal_number)$
  $endif$
  $if(signatory_mobile)$
  <br/>Mobile: $signatory_mobile$
  $endif$
  $if(signatory_dob)$
  <br/>Date of birth: $signatory_dob$
  $endif$
  $if(signatory_pid)$
  <br/>PID: $signatory_pid$
  $endif$
  $if(signatory_ip)$
  <br/>IP: $signatory_ip$
  $endif$
  $if(signatory_bank_name)$
  <br/>Bank: $signatory_bank_name$
  $endif$
  $if(signatory_email)$
  <br/>Email: $signatory_email$
  $endif$
  $if(signatory_distinguished_name)$
  <br/>Signatory distinguished Name: $signatory_distinguished_name$
  $endif$
  $if(issuer_distinguished_name)$
  <br/>Issuer distinguished Name: $issuer_distinguished_name$
  $endif$
  $if(provider_customer_id)$
  <br/>EID Provider Customer ID: $provider_customer_id$
  $endif$
</p>
<br/>
$if(signature)$
<p>Signature: <div style="word-break:break-word;">$personalNumber(value=signature)$</div></p>
$endif$
$if(ocsp_response)$
<p>Online Certificate Status Protocol (OCSP) Response:
  <div style="word-break:break-word;">$ocsp_response$</div>
</p>
$endif$
###
provider=$if(provider_sebankid)$CGI$elseif(provider_nobankid)$Nets$elseif(provider_nobankid_eidservice)$Vipps$elseif(provider_dknemid)$Nets$elseif(provider_fitupas)$Nets$elseif(provider_verimi)$Verimi$elseif(provider_idin)$iDIN$elseif(provider_onfido)$$if(onfido_document)$Onfido (document check only)$else$Onfido (document check + facial comparison)$endif$$endif$
###
providerType=$if(provider_sebankid)$Swedish BankID$elseif(provider_nobankid)$Norwegian BankID$elseif(provider_nobankid_eidservice)$Norwegian BankID$elseif(provider_dknemid)$Danish NemID$elseif(provider_fitupas)$Finnish TUPAS$elseif(provider_onfido)$Onfido$endif$
###
AuthenticatedToViewEvidenceArchive=*
$if(provider_sebankid)$
  $_AuthenticatedToViewEvidenceArchiveSEBankID()$
$elseif(provider_nobankid)$
  $_AuthenticatedToViewEvidenceArchiveNOBankID()$
$elseif(provider_dknemid)$
  $_AuthenticatedToViewEvidenceArchiveDKNemID()$
$elseif(provider_fitupas)$
  $_AuthenticatedToViewEvidenceArchiveFITupas()$
$elseif(provider_sms_pin)$
  $_AuthenticatedToViewEvidenceArchiveSMSPin()$
$elseif(provider_verimi)$
  $_AuthenticatedToViewEvidenceArchiveVerimi()$
$elseif(provider_idin)$
  $_AuthenticatedToViewEvidenceArchiveIDIN()$
$endif$
###
UpdateMobileAfterIdentificationToViewWithNetsLog=The party <span class="signatory"/> wrote the number "$oldphone$" in the field "Mobile". Scrive eSign changed it from "$oldphone$" to "$newphone$".
###
UpdateSsnAfterAuthenticationToViewWithNetsLog=The signatory <span class="signatory"/> changed the text in the text field "ID number" from blank to "$new_ssn$". Scrive eSign permanently assigned this ID number for this signatory.
###
AuthorAttachmentHashComputedLog=The attachment "$attachment_name$" will not be merged with the main document. To be able to confirm the integrity of "$attachment_name$" it was hashed using the SHA-256 method.
<br/>
<br/>
<p>Hash: <div>$hash$</div></p>
###
PageHighlightingAddedLog=Highlighting was added to page $pageno$ while <span class="signatory"/> was viewing.
###
PageHighlightingClearedLog=Highlighting was removed from page $pageno$ while <span class="signatory"/> was viewing.
###
ConsentQuestionAnsweredLog=The party <span class="signatory"/> answered the question "$question$" by "$response$".
###
ConsentQuestionAnsweredWithDescriptionLog=The party <span class="signatory"/> answered the question "$question$" by "$response$". The question's detailed description had the title "$description_title$" and the text "$description_text$".
###
ApprovedByApproverPartyEvidenceArchive=$_ApprovedByApproverPartyEvidenceArchive()$
###
ApprovedByApproverPartyEvidenceLog=The approver party <span class="signatory"/> approved the document.
###
ForwardedSigningEvidenceLog=<span class="actor"/> forwarded the document to <span class="signatory"/>.
###
ForwardedSigningEvidenceArchive=$_ForwardedSigningEvidenceArchive()$
###
ConfirmationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the confirmation sent via email to <span class="signatory"/> was delivered.
###
ConfirmationUndeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the confirmation sent via email to <span class="signatory"/> could not be delivered and continued attempts were cancelled.
