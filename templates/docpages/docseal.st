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
offersealingtexts=
SealingTexts
    {   verificationTitle="$_offersealingtextsverificationTitle()$"
      , docPrefix="$_sealingtextsdocPrefix()$"
      , signedText="$_offersealingtextssignedText()$"
      , partnerText="$_offersealingtextspartnerText()$"
      , secretaryText="$_offersealingtextssecretaryText()$"
      , documentText="$_documentText()$"
      , orgNumberText="$_offersealingtextsorgNumberText()$"
      , personalNumberText="$_personalNumberText()$"
      , eventsText="$_offersealingtextseventsText()$"
      , dateText="$_offersealingtextsdateText()$"
      , historyText="$_offersealingtextshistoryText()$"
      , verificationFooter="$_verificationFooter()$"
    }
#####
ordersealingtexts=
SealingTexts
    {   verificationTitle="$_ordersealingtextsverificationTitle()$"
      , docPrefix="$_sealingtextsdocPrefix()$"
      , signedText="$_ordersealingtextssignedText()$"
      , partnerText="$_ordersealingtextspartnerText()$"
      , secretaryText="$_ordersealingtextssecretaryText()$"
      , documentText="$_documentText()$"
      , orgNumberText="$_ordersealingtextsorgNumberText()$"
      , personalNumberText="$_personalNumberText()$"
      , eventsText="$_ordersealingtextseventsText()$"
      , dateText="$_ordersealingtextsdateText()$"
      , historyText="$_ordersealingtextshistoryText()$"
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
