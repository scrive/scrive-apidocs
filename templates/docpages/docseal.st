#####
contractsealingtexts=
SealingTexts
    {   verificationTitle="$_contractsealingtexts()$"
      , docPrefix="$_sealingtextsdocPrefix()$"
      , signedText="$_contractsealingtextssignedText()$"
      , partnerText="$_contractsealingtextspartnerText()$"
      , secretaryText="$_contractsealingtextssecretaryText()$"
      , documentText="$_documentText()$"
      , orgNumberText="$_contractsealingtextsorgNumberText()$"
      , personalNumberText="$_personalNumberText()$"
      , eventsText="$_contractsealingtextseventsText()$"
      , dateText="$_contractsealingtextsdateText()$"
      , historyText="$_contractsealingtextshistoryText()$"
      , verificationFooter="$_verificationFooter()$"
    }
#####
signMethod=
        $if(!provider)$
          $if(email)$
            $_signMethodEmail()$
          $elseif(pad)$
            $_signMethodPad()$
          $elseif(api)$
            $_signMethodAPI()$
          $elseif(mobile)$
            $_signMethodMobile()$
          $elseif(email_mobile)$
            $_signMethodEmailAndMobile()$
          $endif$
        $elseif(bankid)$
            $_signMethodElegBankID()$
        $elseif(norder)$
            $_signMethodElegNordea()$
        $elseif(telia)$
            $_signMethodElegTelia()$
        $else$
            $_signMethodEleg()$
        $endif$
###
fstnameEmptyFieldsText=$_firstname()$
####
sndnameEmptyFieldsText=$_lastname()$
####
emailEmptyFieldsText=$_email()$
####
companyEmptyFieldsText=$_company()$
####
companynumberEmptyFieldsText=$_orgnumber()$
####
personalnumberEmptyFieldsText=$_personnumber()$
####
