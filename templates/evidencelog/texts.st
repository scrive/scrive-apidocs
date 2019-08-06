AttachExtendedSealedFileEvidenceLog=Scrive eSign sealed the Evidence Package (as defined in the Service Description) with a keyless digital signature.
###
AttachExtendedSealedFileEvidenceArchive=$_AttachExtendedSealedFileEvidenceTextSimple()$
###
###
AttachGuardtimeSealedFileEvidenceLog=Scrive eSign sealed the final PDF with a digital signature.
###
AttachGuardtimeSealedFileEvidenceArchive=$_AttachGuardtimeSealedFileEvidenceTextSimple()$
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
ChangeAuthenticationToViewMethodStandardToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “no extra authentication” to “PIN by SMS”.
###
ChangeAuthenticationToViewMethodNOBankIDToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Norwegian BankID” to “PIN by SMS”.
###
ChangeAuthenticationToViewMethodSEBankIDToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Swedish BankID” to “PIN by SMS”.
###
ChangeAuthenticationToViewMethodDKNemIDToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Danish NemID” to “PIN by SMS”.
###
ChangeAuthenticationToViewMethodSMSPinToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “PIN by SMS” to “no extra authentication”.
###
ChangeAuthenticationToViewMethodSMSPinToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “PIN by SMS” to “Norwegian BankID”.
###
ChangeAuthenticationToViewMethodSMSPinToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “PIN by SMS” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodSMSPinToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “PIN by SMS” to “Danish NemID”.
###
ChangeAuthenticationToViewMethodStandardToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “no extra authentication” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewMethodNOBankIDToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Norwegian BankID” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewMethodSEBankIDToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Swedish BankID” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewMethodDKNemIDToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Danish NemID” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewMethodSMSPinToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “PIN by SMS” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewMethodFITupasToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Finnish TUPAS” to “no extra authentication”.
###
ChangeAuthenticationToViewMethodFITupasToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Finnish TUPAS” to “Norwegian BankID”.
###
ChangeAuthenticationToViewMethodFITupasToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Finnish TUPAS” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodFITupasToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Finnish TUPAS” to “Danish NemID”.
###
ChangeAuthenticationToViewMethodFITupasToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Finnish TUPAS” to “PIN by SMS”.
###
ChangeAuthenticationToViewArchivedMethodStandardToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “no extra authentication” to “Swedish BankID”.
###
ChangeAuthenticationToViewArchivedMethodStandardToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “no extra authentication” to “Norwegian BankID”.
###
ChangeAuthenticationToViewArchivedMethodSEBankIDToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Swedish BankID” to “no extra authentication”.
###
ChangeAuthenticationToViewArchivedMethodSEBankIDToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Swedish BankID” to “Norwegian BankID”.
###
ChangeAuthenticationToViewArchivedMethodNOBankIDToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Norwegian BankID” to “no extra authentication”.
###
ChangeAuthenticationToViewArchivedMethodNOBankIDToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Norwegian BankID” to “Swedish BankID”.
###
ChangeAuthenticationToViewArchivedMethodDKNemIDToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Danish NemID” to “no extra authentication”.
###
ChangeAuthenticationToViewArchivedMethodDKNemIDToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Danish NemID” to “Swedish BankID”.
###
ChangeAuthenticationToViewArchivedMethodDKNemIDToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Danish NemID” to “Norwegian BankID”.
###
ChangeAuthenticationToViewArchivedMethodStandardToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “no extra authentication” to “Danish NemID”.
###
ChangeAuthenticationToViewArchivedMethodSEBankIDToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Swedish BankID” to “Danish NemID”.
###
ChangeAuthenticationToViewArchivedMethodNOBankIDToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Norwegian BankID” to “Danish NemID”.
###
ChangeAuthenticationToViewArchivedMethodStandardToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “no extra authentication” to “PIN by SMS”.
###
ChangeAuthenticationToViewArchivedMethodNOBankIDToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Norwegian BankID” to “PIN by SMS”.
###
ChangeAuthenticationToViewArchivedMethodSEBankIDToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Swedish BankID” to “PIN by SMS”.
###
ChangeAuthenticationToViewArchivedMethodDKNemIDToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Danish NemID” to “PIN by SMS”.
###
ChangeAuthenticationToViewArchivedMethodSMSPinToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “no extra authentication”.
###
ChangeAuthenticationToViewArchivedMethodSMSPinToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “Norwegian BankID”.
###
ChangeAuthenticationToViewArchivedMethodSMSPinToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “Swedish BankID”.
###
ChangeAuthenticationToViewArchivedMethodSMSPinToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “Danish NemID”.
###
ChangeAuthenticationToViewArchivedMethodStandardToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “no extra authentication” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewArchivedMethodNOBankIDToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Norwegian BankID” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewArchivedMethodSEBankIDToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Swedish BankID” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewArchivedMethodDKNemIDToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Danish NemID” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewArchivedMethodSMSPinToFITupasEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewArchivedMethodFITupasToStandardEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Finnish TUPAS” to “no extra authentication”.
###
ChangeAuthenticationToViewArchivedMethodFITupasToNOBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Finnish TUPAS” to “Norwegian BankID”.
###
ChangeAuthenticationToViewArchivedMethodFITupasToSEBankIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Finnish TUPAS” to “Swedish BankID”.
###
ChangeAuthenticationToViewArchivedMethodFITupasToDKNemIDEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Finnish TUPAS” to “Danish NemID”.
###
ChangeAuthenticationToViewArchivedMethodFITupasToSMSPinEvidenceLog = The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Finnish TUPAS” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodDKNemIDToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Danish NemID” to "Norwegian BankID”.
###
ChangeAuthenticationToSignMethodDKNemIDToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Danish NemID” to “no extra authentication”.
###
ChangeAuthenticationToSignMethodDKNemIDToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Danish NemID” to “Swedish BankID”.
###
ChangeAuthenticationToSignMethodDKNemIDToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Danish NemID” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodNOBankIDToDKNemIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to "Danish NemID”.
###
ChangeAuthenticationToSignMethodNOBankIDToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to "no extra authentication”.
###
ChangeAuthenticationToSignMethodNOBankIDToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodNOBankIDToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Norwegian BankID” to “Swedish BankID”.
###
ChangeAuthenticationToSignMethodStandardToDKNemIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “Danish NemID”.
###
ChangeAuthenticationToSignMethodStandardToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “Norwegian BankID”.
###
ChangeAuthenticationToSignMethodStandardToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “Swedish BankID”.
###
ChangeAuthenticationToSignMethodStandardToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “no extra authentication” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodSEBankIDToDKNemIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “Danish NemID”.
###
ChangeAuthenticationToSignMethodSEBankIDToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “Norwegian BankID”.
###
ChangeAuthenticationToSignMethodSEBankIDToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “no extra authentication”.
###
ChangeAuthenticationToSignMethodSEBankIDToSMSEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “Swedish BankID” to “PIN by SMS”.
###
ChangeAuthenticationToSignMethodSMSToDKNemIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to "Danish NemID”.
###
ChangeAuthenticationToSignMethodSMSToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to "Norwegian BankID”.
###
ChangeAuthenticationToSignMethodSMSToStandardEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to “no extra authentication”.
###
ChangeAuthenticationToSignMethodSMSToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to sign method for <span class="signatory"/> from “PIN by SMS” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodVerimiToSMSPinEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Verimi” to “PIN by SMS”.
###
ChangeAuthenticationToViewMethodVerimiToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Verimi” to “Swedish BankID”.
###
ChangeAuthenticationToViewMethodVerimiToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Verimi” to “Norwegian BankID”.
###
ChangeAuthenticationToViewMethodVerimiToDKNemIDEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Verimi” to “Danish NemID”.
###
ChangeAuthenticationToViewMethodVerimiToFITupasEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Verimi” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewMethodVerimiToStandardEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Verimi” to “no extra authentication”.
###
ChangeAuthenticationToViewMethodStandardToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “no extra authentication” to “Verimi”.
###
ChangeAuthenticationToViewMethodSEBankIDToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from Swedish BankID to “Verimi”.
###
ChangeAuthenticationToViewMethodNOBankIDToVerimeEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from Norwegian BankID to “Verimi”.
###
ChangeAuthenticationToViewMethodDKNemIDToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Danish NemID” to “Verimi”.
###
ChangeAuthenticationToViewMethodFITupasToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “Finnish TUPAS” to “Verimi”.
###
ChangeAuthenticationToViewMethodSMSPinToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view method for <span class="signatory"/> from “PIN by SMS” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodVerimiToSMSPinEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodVerimiToSEBankIDEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Verimi” to “Swedish BankID”.
###
ChangeAuthenticationToViewArchivedMethodVerimiToNOBankIDEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Verimi” to "Norwegian BankID".
###
ChangeAuthenticationToViewArchivedMethodVerimiToDKNemIDEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Verimi” to “Danish NemID”.
###
ChangeAuthenticationToViewArchivedMethodVerimiToFITupasEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Verimi” to “Finnish TUPAS”.
###
ChangeAuthenticationToViewArchivedMethodVerimiToStandardEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Verimi” to “no extra authentication”.
###
ChangeAuthenticationToViewArchivedMethodStandardToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “no extra authentication” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodSEBankIDToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Swedish BankID” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodNOBankIDToVerimeEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Norwegian BankID” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodDKNemIDToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Danish NemID” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodFITupasToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “Finnish TUPAS” to “Verimi”.
###
ChangeAuthenticationToViewArchivedMethodSMSPinToVerimiEvidenceLog=The user <span class="actor"/> changed authentication to view archived method for <span class="signatory"/> from “PIN by SMS” to “Verimi”.
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
$if(provider_dknemid)$
<p>Signing transaction with $provider()$ was restricted to ID number: $if(signatory_personal_number_from_signlink)$$personalNumber(value=signatory_personal_number_from_signlink)$$else$$personalNumber(value=signatory_personal_number)$$endif$</p>
<p>Data returned from $provider()$:
  <br/>Name: $signatory_name$
  $if(signatory_personal_number)$<br/>ID number: $personalNumber(value=signatory_personal_number)$$endif$
  $if(signatory_ip)$<br/>IP: $signatory_ip$$endif$
</p>
$endif$
<br/>
<p>Signature: <div>$personalNumber(value=signature)$</div></p>
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
</p>
<br/>
$if(signature)$
<p>Signature: <div>$personalNumber(value=signature)$</div></p>
$endif$
$if(ocsp_response)$
<p>Online Certificate Status Protocol (OCSP) Response: <div>$ocsp_response$</div></p>
$endif$
###
provider=$if(provider_sebankid)$CGI$elseif(provider_nobankid)$Nets$elseif(provider_dknemid)$Nets$elseif(provider_fitupas)$Nets$elseif(provider_verimi)$Verimi$endif$
###
providerType=$if(provider_sebankid)$Swedish BankID$elseif(provider_nobankid)$Norwegian BankID$elseif(provider_dknemid)$Danish NemID$elseif(provider_fitupas)$Finnish TUPAS$endif$
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
ForwardedSigningEvidenceArchive=$_ForwardedSigningEvidenceArchive()$.
###
ConfirmationDeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the confirmation sent via email to <span class="signatory"/> was delivered.
###
ConfirmationUndeliveredByEmailLog=Scrive eSign’s external email delivery system reported that the confirmation sent via email to <span class="signatory"/> could not be delivered and continued attempts were cancelled.
