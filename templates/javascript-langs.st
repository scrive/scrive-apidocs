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

           
        }
####
