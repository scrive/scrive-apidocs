#### 
#### This module puts in global context object 'localisation'
#### If some script requires some string to be shown to the user 
#### it just uses this module
####
javascriptLocalisation = 
    var localization = 
        {
              cantRemoveActiveDocuments : "Det går inte att radera dokument som är skickade eller öppna, var vänliga återkalla dokumentet först."
            , cantRemoveActiveOffers : "Det går inte att radera offer som är skickade eller öppna, var vänliga återkalla offerter först."
            , loadingFile : "Laddar upp . . ."
            , problemWithPDF : "Problem med PDF"
            , repetedEmailAddress : "Du kan ej använda samma e-post för mer än 1 användare" 
            , offerSignatory : "Mottagare" 
            , secretaryCantSign : "Du kan inte underteckna när du är sekreterare. Om du vill underteckna, gå tillbaks till steg 2 och byt roll."
            , cantSendReminder: function(listtype){
                    return "Det går inte att skicka påminnelser för " + listtype + " som inte är skickade eller öppna."
                    }
            , addRequiredAttachments : "Var vänlig bifoga begärda bilagor."
            , setAllFields : "Var vänlig namnge alla skapade fält"
            , multipleSignatory : "Massutskick"
            , contractSignatory : function(idx) {return "Part " + idx}
            , nonsignatory : "Ej undertecknande part"
            , offerAuthor : "Avsändare"
            , youMustSetParnerEmail : "Du måste ange e-post till motpart."
            , emptyOrNotValidEmail :  "Du har inte skrivit in en e-post eller e-posten är felaktig. Vänligen försök igen."
            , atLeastOneSignatoryRequired : "Du måste ha minst en undertecknande part."
            , mustFillFieldsBeforeSigning : "Du måste fylla i tomma fält innan du kan underteckna."
            , backToStepTwoAndFillPersonNumber : "Var vänlig gå tillbaka till steg 2 och fyll i personnummer."
            , missingNames : "Du har inte namngett alla fält. Vänligen försök igen." 
            , missingSignatoryNames : "Du har inte skrivit in något namn på en eller flera motparter. Vänligen försök igen."
            , yourSigningPluginFailed : "Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör. "
            , startingSaveSigning : "Inleder säker signering . . ."
            , noNordeaInstalled  : "Du har inte Nordeas e-legitimation installerad. Du kan ladda ned Nordeas e-legitimation från Nordeas internetbank."
            , noTeliaInstalled  : "Du har inte Telias e-legitimation installerad."
            , noBankIdInstalled  : "Du har inte BankID installerat. Du kan ladda ned BankID från din internetbank. Följande banker tillhandahåller BankID via internetbanken."
            , verifyingSignature : "Verifierar signatur..."
            , tbsGenerationMessage : function(doctitle, docid){
                    return 'Du undertecknar "' + doctitle + '" med transaktionsnummer "' + docid + '". Undertecknande parter är:'
                     }
            , differentSignatories :  "Various signatories."
            , ie6NotSupported : "Din webbläsare, Internet Explorer 6, stöds inte längre. Vänligen uppgradera till en modern webbläsare"
            , youCanNotInviteSameEmail : "Du kan inte bjuda in underkonton vars e-post inte har samma URL som du har i din e-post."
            , fstname : "Förnamn"
            , sndname : "Efternamn"
            , companyposition : "Befattning"
            , phone : "Telefonnummer"
            , email : "Email"
            , niceEmail : "Personens e-mail"
            , company : "Företag"
            , personamNumber : "Pers. nr"
            , companyNumber : "Org. nr"
            , loadingTweets : "Laddar tweets.."
            , fieldName : "Namnge fältet"
   }
####
