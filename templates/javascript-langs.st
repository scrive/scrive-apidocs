#### 
#### This module puts in global context object 'localisation'
#### If some script requires some string to be shown to the user 
#### it just uses this module
####
javascriptLocalisation = 
    var localization = 
        {
       loadingFile : "$_loadingFile()$"
             , problemWithPDF : "$_problemWithPDF()$"
             , repetedEmailAddress : "$_repetedEmailAddress()$" 
             , offerSignatory : "$_offerSignatory()$" 
             , secretaryCantSign : "$_secretaryCantSign()$"
             , cantSendReminder: function(listtype){
                    return "$_cantSendReminder1()$" + listtype + "$_cantSendReminder2()$"
                    }
             , addRequiredAttachments : "$_addRequiredAttachments()$"
             , setAllFields : "$_setAllFields()$"
             , multipleSignatory : "$_multipleSignatory()$"
             , contractSignatory : function(idx) {return "$_contractSignatory()$" + idx}
             , nonsignatory : "$_nonsignatory()$"
             , offerAuthor : "$_offerAuthor()$"
             , author : "$_sender()$"
             , youMustSetParnerEmail : "$_youMustSetParnerEmail()$"
             , emptyOrNotValidEmail :  "$_emptyOrNotValidEmail()$"
             , atLeastOneSignatoryRequired : "$_atLeastOneSignatoryRequired()$"
             , mustFillFieldsBeforeSigning : "$_mustFillFieldsBeforeSigning()$"
             , backToStepTwoAndFillPersonNumber : "$_backToStepTwoAndFillPersonNumber()$"
             , missingNames : "$_missingNames()$" 
             , missingSignatoryNames : "$_missingSignatoryNames()$"
             , yourSigningPluginFailed : "$_yourSigningPluginFailed()$"
             , startingSaveSigning : "$_startingSaveSigning()$"
             , noNordeaInstalled  : "$_noNordeaInstalled()$"
             , noTeliaInstalled  : "$_noTeliaInstalled()$"
             , noBankIdInstalled  : "$_noBankIdInstalled()$"
             , verifyingSignature : "$_verifyingSignature()$"
             , tbsGenerationMessage : function(doctitle, docid){
                    return  '$_tbsGenerationMessage1()$"' + doctitle + '"$_tbsGenerationMessage2()$"' + docid + '"$_tbsGenerationMessage3()$'
                     }
             , differentSignatories :  "$_differentSignatories()$"
             , ie6NotSupported : "$_ie6NotSupported()$"
             , youCanNotInviteSameEmail : "$_youCanNotInviteSameEmail()$"
             , fstname : "$_firstname()$"
             , sndname : "$_lastname()$"
             , companyposition : "$_companyposition()$"
             , phone : "$_phone()$"
             , email : "$_email()$"
             , niceEmail : "$_niceEmail()$"
             , company : "$_company()$"
             , personamNumber : "$_personnumber()$"
             , companyNumber : "$_orgnumber()$"
             , loadingTweets : "$_loadingTweets()$"
             , fieldName : "$_fieldName()$"
             , multiPartPartner : "Parter"
             , noNamePerson : "Namnlös"
             , senderName : "Avsändare namn på motpart"
             , senderTitle : "Avsändare  titel, företag"
             , senderNumber :  "Avsändare Orgnr/Persnr"
             , senderEmail :  "Avsändare personens e-mail"
             , partnerName : "Namn på motpart"
             , partnerTitle : "Titel, företag"
             , partnerNumber : "Orgnr/Persnr"
             , partnerEmail : "Personens e-mail"
             , statusToolTip: {
                   draft: "$_tooltipdraft()$"
                 , cancelled : "$_tooltipcancelled()$"
                 , sent : "$_tooltipsent()$"
                 , delivered : "$_tooltipdelivered()$"
                 , reviewed : "$_tooltipreviewed()$"
                 , read : "$_tooltipread()$"
                 , opened : "$_tooltipopened()$"
                 , signed : "$_tooltipsigned()$"
             }
             , select : "$_options()$"
             , of : "$_of()$"
   }
####
