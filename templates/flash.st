### Some comment here
flashDocumentDraftSaved=
    Du har sparat dokumentet.

######
flashDocumentTemplateSaved=
    Din mall är sparad.

######
flashRemindMailSentNotSigned=
    En påminnelse har skickats till $personname$

######
flashRemindMailSentSigned=
    Dokumentet har skickats till $personname$

######
flashMessageCannotCancel=
    Motparten har redan undertecknat dokumentet. Du kan inte återkalla inbjudan.

######
flashMessageCanceled=
    $if(contract)$
     Du har nu återkallat inbjudan att underteckna dokumentet och ett meddelande om återkallelsen har skickats till samtliga parter.
    $elseif(offer)$
     Du har nu återkallat offerten och ett meddlande om återkallelsen har skickats till samtliga parter
    $endif$
#####
flashMessageNoAccountType=
     Du måste välja kontotyp.
#####
flashMessageInvalidAccountType=
    Den kontotyp du försöker att välja är inte tillåten.
#####
flashMessageMustAcceptTOS=
    För att kunna använda tjänsten måste du acceptera SkrivaPå Allmänna Villkor.

####
flashMessageUserDetailsSaved=
    <div>Dina kontoändringar har sparats.</div>

####        
flashMessageBadOldPassword=
    Du har skrivit in fel nuvarande lösenord

####
flashMessagePasswordsDontMatch=
    "Nytt lösenord" matchar inte med "Upprepa lösenord" 

###
flashDocumentRestarted=
    $if(contract)$
    Du har startat om dokumentet och kan nu underteckna och skicka inbjudan på nytt.
    $elseif(offer)$
    Du har startat om offerten och kan nu skicka den på nytt
    $endif$
###
flashMessageUserPasswordChanged=Ditt lösenord har ändrats.

###
flashMessagePasswordChangeLinkNotValid=
    Vi beklagar. Länken är inte längre giltig.<BR>
     Vänligen använd "Glömt lösenord" för att generera en ny länk.

###
flashMessageUserWithSameEmailExists=
    En användare med samma e-post existerar redan.

###
flashMessageViralInviteSent=
    Inbjudan har skickats!

###
flashMessageOtherUserSentInvitation=
    Inbjudan har redan skickats av en annan användare.
###
flashMessageNoRemainedInvitationEmails=
    Du har redan skickat 10 inbjudningar till denna e-post. Vänligen försök senare.
###
flashMessageActivationLinkNotValid=
    Vi beklagar. Länken är inte längre giltig.<BR>
    Vänligen använd "Glömt lösenord" för att generera en ny länk.

###
flashMessageUserActivated=
    Välkommen till SkrivaPå!

###
flashMessageUserAlreadyActivated=
    Du har redan aktiverat detta konto.

###
flashMessageChangePasswordEmailSend=Ett nytt lösenord har blivit skickat till din e-post.
###
flashMessageNoRemainedPasswordReminderEmails=
    Du har redan begärt lösenordsbyte 10 gånger i rad. Vänligen försök igen senare.
####
flashMessageNewActivationLinkSend=
    Vi har skickat din kontoinbjudan igen. Vänligen gå tillbaks till din inkorg och klicka på länken i mailet för att aktivera ditt konto.

###
flashMessageUserSignupDone=Ditt konto har skapats! Vi har skickat ett mail med dina användaruppgifter till din inkorg.
###
flashAuthorSigned=Dokumentet har undertecknats.
###
flashMessageLoginPageRedirectReason=
$if(notlogged)$
  Du är inte inloggad.
$endif$
$if(notsu)$
  Du har inte rätt behörighet att se denna sida.
$endif$
$if(invloginfo)$
  Felaktig e-post eller lösenord.
$endif$
###
###
emailFieldName=e-post
###
passwordFieldName=lösenord
###
nameFieldName=namn
###
firstNameFieldName=förnamn
###
secondNameFieldName=efternamn
###
companyNameFieldName=företagsnamn
###
companyNumberFieldName=organisationsnummer
###
addressFieldName=adress
###
positionFieldName=position
###
checkBoxFieldName=check box
###
daysToSignFieldName=days to sign
###
idFieldName=id
###
fieldNameFieldName=namn på fält
###
fieldValueFieldName=värde på fält
###
placeFieldName=place
###
inviteTextFieldName=invite text
###
flashMessageNeedsLetterAndDigit=$fieldnametitlecase$ måste innehålla åtminstone två bokstäver och två nummer.
###
flashMessageInvalidFormat=$fieldnametitlecase$ är ej korrekt formaterad.
###
flashMessageNumberBelowMinimum=$fieldnametitlecase$ must be a minimum of $min$.
###
flashMessageNumberAboveMaximum=$fieldnametitlecase$ can be a maximum of $max$.
###
flashMessageNotAValidInteger=$fieldnametitlecase$ should be a number.
###
flashMessageInvalidSpaceInInput=Blankrad är inte tillåtet i $fieldnamelowercase$.
###
flashMessageInvalidUnprintableCharsInInput=$fieldnametitlecase$ innehåller ej tillåtna tecken. 
###
flashMessageInvalidPrintableCharsInInput=$fieldnametitlecase$ innehåller ej tillåtna tecken.
###
flashMessageInputLessThanMinLength=$fieldnametitlecase$ måste innehålla minst $minlength$ tecken.
###
flashMessageInputExceedsMaxLength=$fieldnametitlecase$ får max innehålla $maxlength$ tecken.
###
flashMessageMissingRequiredField=Du har missat att fylla i ett obligatoriskt fält.
###
flashMessageFailedToParseCSV=Misslyckad tolkning av .csv, var vänlig och kontrollera och ladda upp igen.
###
flashMessageCSVHasTooManyRows=Vi beklagar, men vi har begränsat denna fuktion til $maxrows$ rader.
<br/>Var vänlig kontakta vår support om du behöver infoga fler än $maxrows$ rader.
###
flashMessageNoDataInCSV=Kunde inte hitta några data i filen.
###
flashMessageNoDataInCSVApartFromHeader=Kunde inte hitta några data i filen, kunde bara hitta titlar.
###
flashMessageRowLessThanMinColCount=Raden behöver åtminstone $mincols$ kolumner.
###
flashMessageRowGreaterThanMaxColCount=Raden kan inte ha mer än $maxcols$ kolumner.
###
flashMessageFirstNameIsRequired=Förnamn krävs.
###
flashMessageSecondNameIsRequired=Efternamn krävs.
###
flashMessageEmailIsRequired=E-post krävs.
###
flashMessageNumberIsRequired=Nummer krävs.
###
flashMessageBulkContractRemindsSent=Påminnelse har skickats till alla parter som ännu inte signerat.
###
flashMessageNoBulkContractRemindsSent=Tyvärr, det finns inga parter som återstår att underteckna.
###
flashMessageBulkOfferRemindsSent=Påminnelse har skickats till alla mottagare som ännu inte accepterat.
###
flashMessageNoBulkOfferRemindsSent=Tyvärr, det finns inga mottagare som återstår att acceptera.
###
flashMessageContractArchiveDone=Valda dokument har blivit raderade.
###
flashMessageOfferArchiveDone=Valda offerter har blivit raderade.
###
flashMessageTemplateArchiveDone=Valda mallar har blivit raderade.
###
flashMessageAttachmentArchiveDone=Valda bilagor har blivit raderade.
###
flashMessageThanksForTheQuestion=Ditt meddelande har skickats! Vi återkommer inom 24h.
###
flashMessageInvalidCSV=Det uppstod ett problem med datan i din .csv. Var vänling och kontrollera och ladda upp igen.
###
flashMessageCSVSent=Dokumentet är skickat till $doccount$ parter.
###
flashMessageSingleTemplateShareDone=$docname$ har blivit delad inom din organisation.
###
flashMessageMultipleTemplateShareDone=Valda mallar har blivit delade inom din organisation.
###
flashMessageSingleAttachmentShareDone=$docname$ har blivit delad inom din organisation.
###
flashMessageMultipleAttachmentShareDone=Valda bilagor har blivit delade inom din organisation.
###
flashMessageUserInvitedAsSubaccount=Din förfrågan har skickats!
###
flashMessageUserHasBecomeSubaccount=Du är nu underkonto under
huvudkontot $supervisor.fullnamePlusEmail$ och rättigheterna till dina
dokument har överförts till huvudkontot.
###
flashMessageAccountActivatedFromSign=Ditt original har arkiverats hos SkrivaPå.
###
flashMessageAccountRemovedFromSign=Ditt original är borttaget från SkrivaPå.
###
flashMessageOnlyHaveRightsToViewDoc=Du har endast rättigheter att se detta dokument.
###
flashMessagePleaseSignWithEleg=Du undertecknar dokumentet längst ned. Det krävs e-legitimation för att underteckna.
###
flashMessagePleaseSignContract=Underteckna dokumentet längst ned på sidan.
###
flashMessagePleaseSignOffer=Du kan acceptera offerten längst ned på sidan.
###
flashMessageUserHasLiveDocs=Unable to remove account(s) as there are some remaining documents that need to be finalised (spec/translate me please).
###
flashMessageAccountsDeleted=Account(s) successfully deleted (spec/translate me please).
###
