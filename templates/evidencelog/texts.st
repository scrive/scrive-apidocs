AttachExtendedSealedFileEvidenceLog=Scrive eSign sealed the Evidence Package (as defined in the Service Description) with a keyless digital signature.
###
AttachExtendedSealedFileEvidenceArchive=$_AttachExtendedSealedFileEvidenceTextSimple()$
###
AttachExtendedSealedFileEvidenceVerificationPages=$_AttachExtendedSealedFileEvidenceTextSimple()$
###
###
AttachGuardtimeSealedFileEvidenceLog=Scrive eSign sealed the final PDF with a digital signature.
###
AttachGuardtimeSealedFileEvidenceArchive=$_AttachGuardtimeSealedFileEvidenceTextSimple()$
###
AttachGuardtimeSealedFileEvidenceVerificationPages=$_AttachGuardtimeSealedFileEvidenceTextSimple()$
###
###
AttachSealedFileEvidenceLog=Scrive eSign prepared the final PDF, including evidence collected by Scrive eSign, before sealing with a digital signature.
###
AttachSealedFileEvidenceArchive=$_AttachSealedFileEvidenceTextSimpleArchive()$
###
AttachSealedFileEvidenceVerificationPages=$_AttachSealedFileEvidenceTextSimpleNotArchive()$
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
AutomaticReminderSentVerificationPages=$AutomaticReminderSentSimple()$
###
###
CancelDocumentEvidenceLog=The user <span class="actor"/> withdrew the invitation to sign and Scrive eSign disabled the possibility to sign for all parties.
###
CancelDocumentEvidenceArchive=$_CancelDocumentEvidenceTextSimpleArchive()$
###
CancelDocumentEvidenceVerificationPages=$_CancelDocumentEvidenceTextSimpleNotArchive()$
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
ChangeAuthenticationToViewMethodStandardToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “no extra authentication” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodStandardToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “no extra authentication” to “Norwegian BankID”.
###
ChangeAuthenticationToViewMethodSEBankIDToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Swedish BankID” to “no extra authentication”.
###
ChangeAuthenticationToViewMethodSEBankIDToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Swedish BankID” to “Norwegian BankID”.
###
ChangeAuthenticationToViewMethodNOBankIDToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Norwegian BankID” to “no extra authentication”.
###
ChangeAuthenticationToViewMethodNOBankIDToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Norwegian BankID” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodDKNemIDToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Danish NemID” to “no extra authentication”.
###
ChangeAuthenticationToViewMethodDKNemIDToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Danish NemID” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodDKNemIDToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Danish NemID” to “Norwegian BankID”.
###
ChangeAuthenticationToViewMethodStandardToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “no extra authentication” to “Danish NemID”.
###
ChangeAuthenticationToViewMethodSEBankIDToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Swedish BankID” to “Danish NemID”.
###
ChangeAuthenticationToViewMethodNOBankIDToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Norwegian BankID” to “Danish NemID”.
###
ChangeAuthenticationToSignMethodNOBankIDToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to "no extra authentication”.
###
ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodNOBankIDToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to “Swedish BankID”.
###
ChangeAuthenticationToSignMethodStandardToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “Norwegian BankID”.
###
ChangeAuthenticationToSignMethodStandardToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “Swedish BankID”.
###
ChangeAuthenticationToSignMethodStandardToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “Norwegian BankID”.
###
ChangeAuthenticationToSignMethodSEBankIDToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “no extra authentication”.
###
ChangeAuthenticationToSignMethodSEBankIDToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodSMSToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to "Norwegian BankID”.
###
ChangeAuthenticationToSignMethodSMSToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to “no extra authentication”.
###
ChangeAuthenticationToSignMethodSMSToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to “Swedish BankID”.
###
###
CloseDocumentEvidenceLog=Scrive eSign locked the document from further modifications by the parties.
###
CloseDocumentEvidenceArchive=$_CloseDocumentEvidenceTextSimple()$
###
CloseDocumentEvidenceVerificationPages=$_CloseDocumentEvidenceTextSimple()$

###
DeleteSigAttachmentEvidenceLog=The party <span class="actor"/> deleted the attachment “$name$” previously uploaded by the party <span class="actor"/>.
###
ErrorSealingDocumentEvidenceLog=Due to a system error Scrive eSign failed to finalise the Evidence Package (as defined in the Service Description) before sealing with a digital signature.
###
ReviewInvitationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation to review the document sent via email to <span class="signatory"/> was delivered.
###
SigningInvitationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation to sign the document sent via email to <span class="signatory"/> was delivered.
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
ReviewInvitationDeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation to review the document sent via SMS to <span class="signatory"/> was delivered.
###
SigningInvitationDeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation to sign the document sent via SMS to <span class="signatory"/> was delivered.
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
SigningInvitationEvidenceLogEmail=Scrive eSign sent an invitation to sign the document via email to <span class="signatory"/>. The invitation included a link that when clicked lead to Scrive eSign’s online interface to sign documents. $invitationCustomMessageTextIfThereIsOne()$
###
SigningInvitationEvidenceLogSMS=Scrive eSign sent an invitation to sign the document via SMS to <span class="signatory"/>. The invitation included a link that when clicked lead to Scrive eSign’s online interface to sign documents. $invitationCustomMessageTextIfThereIsOne()$
###
SigningInvitationEvidenceLogEmailSMS=Scrive eSign sent an invitation to sign the document via email and SMS to <span class="signatory"/>. The invitation included a link that when clicked lead to Scrive eSign’s online interface to sign documents. $invitationCustomMessageTextIfThereIsOne()$
###
ReviewInvitationEvidenceLogEmail=Scrive eSign sent an invitation to review the document via email to <span class="signatory"/>. The invitation included a link that when clicked lead to Scrive eSign’s online interface to review documents. $invitationCustomMessageTextIfThereIsOne()$
###
ReviewInvitationEvidenceLogSMS=Scrive eSign sent an invitation to review the document via SMS to <span class="signatory"/>. The invitation included a link that when clicked lead to Scrive eSign’s online interface to review documents. $invitationCustomMessageTextIfThereIsOne()$
###
ReviewInvitationEvidenceLogEmailSMS=Scrive eSign sent an invitation to review the document via email and SMS to <span class="signatory"/>. The invitation included a link that when clicked lead to Scrive eSign’s online interface to review documents. $invitationCustomMessageTextIfThereIsOne()$
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
InvitationUndeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the invitation sent via email to <span class="signatory"/> could not be delivered and continued attempts were cancelled.
###
InvitationUndeliveredByEmailArchive=$_InvitationUndeliveredByEmailTextSimpleArchive()$
###
InvitationUndeliveredByEmailVerificationPages=$_InvitationUndeliveredByEmailTextSimpleNotArchive()$
###
###
InvitationUndeliveredBySMSLog=Scrive eSign’s external SMS delivery system reported that the invitation sent via SMS to <span class="signatory"/> could not be delivered and continued attempts were cancelled.
###
InvitationUndeliveredBySMSArchive=$_InvitationUndeliveredBySMSTextSimpleArchive()$
###
InvitationUndeliveredBySMSVerificationPages=$_InvitationUndeliveredBySMSTextSimpleNotArchive()$
###
MarkInvitationReadEvidenceLog=Scrive eSign’s external email delivery system reported that the invitation sent to $signatory_email$ was opened.
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
PreparationToPendingEvidenceLog=The initiator <span class="actor"/> initiated the signing workflow and Scrive eSign a) locked the signing workflow from further editing, b) set the signing workflow language to $lang$, c) set the signing due date to $timeouttime$ UTC,$if(timezone)$ d) set the time zone to $timezone$$endif$ and e) initiated the signing workflow.
###
PreparationToPendingEvidenceArchive=$_PreparationToPendingEvidenceTextSimpleArchive()$
###
PreparationToPendingEvidenceVerificationPages=$_PreparationToPendingEvidenceTextSimpleNotArchive()$
###
ProlongDocumentEvidenceLog=The user <span class="actor"/> prolonged the signing due date.
###
ProlongDocumentEvidenceArchive=$_ProlongDocumentEvidenceTextArchive()$
###
ProlongDocumentEvidenceVerificationPages=$_ProlongDocumentEvidenceTextNotArchive()$
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
RejectDocumentEvidenceVerificationPages=*
$if(text)$
  $_RejectDocumentEvidenceTextSimpleNotArchiveWithText()$
$else$
  $_RejectDocumentEvidenceTextSimpleNotArchive()$
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
$endif$
###
ReminderSendVerificationPages=$ReminderSendLog()$
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
          $if(provider_sebankid)$
              $_SignDocumentEvidenceTextSimpleNotArchiveElegWithName()$
          $else$
              $_SignDocumentEvidenceTextSimpleNotArchiveNOBankIDWithName()$
          $endif$
      $else$
          $if(provider_sebankid)$
              $_SignDocumentEvidenceTextSimpleNotArchiveEleg()$
          $else$
              $_SignDocumentEvidenceTextSimpleNotArchiveNOBankID()$
          $endif$
      $endif$
$else$
      $_SignDocumentEvidenceTextSimpleNotArchiveStandard()$
$endif$
###
SignatorySignLinkVisitedLog=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to sign documents.
###
SignatoryReviewLinkVisitedLog=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to review documents.
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
TimeoutDocumentEvidenceLog=The due date for signing the document passed. Scrive eSign disabled the possibility to sign for all parties.
###
TimeoutDocumentEvidenceArchive=$_TimeoutDocumentEvidenceTextSimple()$
###
TimeoutDocumentEvidenceVerificationPages=$_TimeoutDocumentEvidenceTextSimple()$
###
AuthorAttachmentAcceptedVerificationPages=$_AuthorAttachmentAcceptedTextNotArchiveSimple()$.
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
<br/>
<p>The text signed in the $providerType$ client was:</p>
<pre style='white-space:normal'>$signed_text$</pre>
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
<br/>
<p>Signature: <div>$signature$</div></p>
$if(provider_sebankid)$
<p>Online Certificate Status Protocol (OCSP) Response:
  <div>$ocsp_response$</div>
</p>
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
CancelDocumenElegEvidenceText=$_CancelDocumenElegEvidenceText()$
###
###
###
VisitedViewForSigningEvidenceArchive=$_VisitedViewForSigningEvidenceArchive()$
###
VisitedViewForSigningEvidenceVerificationPages=$_VisitedViewForSigningEvidenceVerificationPages()$
###
###
VisitedViewForSigningEvidenceLogForSign=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to sign documents.
###
VisitedViewForSigningEvidenceLogForReview=The party <span class="actor"/> opened the document in Scrive eSign’s online interface to review documents.
###
VisitedViewForSigningEvidenceLog=*
$if(signing)$
  $VisitedViewForSigningEvidenceLogForSign()$
$elseif(viewing)$
  $VisitedViewForSigningEvidenceLogForReview()$
$endif$
###
VisitedViewForAuthenticationEvidenceLog=The party <span class="actor"/> opened the view to verify their identity before viewing the document.
###
VisitedViewForAuthenticationEvidenceArchive=$_VisitedViewForAuthenticationEvidenceArchive()$
###
VisitedViewForAuthenticationEvidenceVerificationPages=$_VisitedViewForAuthenticationEvidenceVerificationPages()$
###
AuthenticatedToViewEvidenceLog=<span class="actor"/> verified their $providerType()$ identity through $provider()$ to access the document. $elegVerificationAuth()$
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
</p>
<br/>
$if(signature)$
<p>Signature: <div>$signature$</div></p>
$endif$
$if(ocsp_response)$
<p>Online Certificate Status Protocol (OCSP) Response: <div>$ocsp_response$</div></p>
$endif$
###
provider=$if(provider_sebankid)$CGI$elseif(provider_nobankid)$Nets$elseif(provider_dknemid)$Nets$endif$
###
providerType=$if(provider_sebankid)$Swedish BankID$elseif(provider_nobankid)$Norwegian BankID$elseif(provider_dknemid)$Danish NemID$endif$
###
AuthenticatedToViewEvidenceArchive=*
$if(provider_sebankid)$
  $_AuthenticatedToViewEvidenceArchiveSEBankID()$
$elseif(provider_nobankid)$
  $_AuthenticatedToViewEvidenceArchiveNOBankID()$
$elseif(provider_dknemid)$
  $_AuthenticatedToViewEvidenceArchiveDKNemID()$
$endif$
###
AuthenticatedToViewEvidenceVerificationPages=*
$if(signatory_dob)$
  $if(provider_sebankid)$
    $_AuthenticatedToViewEvidenceVerificationPagesWithDateSEBankID()$
  $elseif(provider_nobankid)$
    $_AuthenticatedToViewEvidenceVerificationPagesWithDateNOBankID()$
  $elseif(provider_dknemid)$
    $_AuthenticatedToViewEvidenceVerificationPagesWithDateDKNemID()$
  $endif$
$else$
  $if(provider_sebankid)$
    $_AuthenticatedToViewEvidenceVerificationPagesWithoutDateSEBankID()$
  $elseif(provider_nobankid)$
    $_AuthenticatedToViewEvidenceVerificationPagesWithoutDateNOBankID()$
  $elseif(provider_dknemid)$
    $_AuthenticatedToViewEvidenceVerificationPagesWithoutDateDKNemID()$
  $endif$
$endif$
###
UpdateMobileAfterIdentificationToViewWithNetsLog=The party <span class="signatory"/> wrote the number "$oldphone$" in the field "Mobile". Scrive eSign changed it from "$oldphone$" to "$newphone$".
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
