#####
contractsealingtexts= 
SealingTexts
    {   verificationTitle="$_contractsealingtexts()$"
      , docPrefix="$_contractsealingtextsdocPrefix()$"
      , signedText="$_contractsealingtextssignedText()$"
      , partnerText="$_contractsealingtextspartnerText()$"
      , secretaryText="$_contractsealingtextssecretaryText()$"
      , orgNumberText="$_contractsealingtextsorgNumberText()$"
      , eventsText="$_contractsealingtextseventsText()$"
      , dateText="$_contractsealingtextsdateText()$"
      , historyText="$_contractsealingtextshistoryText()$"
      , verificationFooter="$_verificationFooter()$"
    }
#####
offersealingtexts=
SealingTexts
    {   verificationTitle="$_offersealingtextsverificationTitle()$"
      , docPrefix="$_offersealingtextsdocPrefix()$"
      , signedText="$_offersealingtextssignedText()$"
      , partnerText="$_offersealingtextspartnerText()$"
      , secretaryText="$_offersealingtextssecretaryText()$"
      , orgNumberText="$_offersealingtextsorgNumberText()$"
      , eventsText="$_offersealingtextseventsText()$"
      , dateText="$_offersealingtextsdateText()$"
      , historyText="$_offersealingtextshistoryText()$"
      , verificationFooter="$_verificationFooter()$"
    }
#####
ordersealingtexts= 
SealingTexts
    {   verificationTitle="$_ordersealingtextsverificationTitle()$"
      , docPrefix="$_ordersealingtextsdocPrefix()$"
      , signedText="$_ordersealingtextssignedText()$"
      , partnerText="$_ordersealingtextspartnerText()$"
      , secretaryText="$_ordersealingtextssecretaryText()$"
      , orgNumberText="$_ordersealingtextsorgNumberText()$"
      , eventsText="$_ordersealingtextseventsText()$"
      , dateText="$_ordersealingtextsdateText()$"
      , historyText="$_ordersealingtextshistoryText()$"
      , verificationFooter="$_verificationFooter()$"
    }
#####
contractLastHistEntry=
$_contractLastHistEntry()$
#####
offerLastHistEntry=
$_offerLastHistEntry()$
####
orderLastHistEntry=
$_orderLastHistEntry()$
####
contractInvitationSentEntry=
    $authorfstname$ $authorsndname$ $_contractInvitationSentEntry()$
    $if(oneSignatory)$
        $_contractInvitationSentEntryIfoneSignatory()$
    $else$
        $_contractInvitationSentEntryIfElse()$
    $endif$ 
    
    $ip$
###
offerInvitationSentEntry=
    $authorfstname$ $authorsndname$ $_offerInvitationSentEntry()$
    $if(oneSignatory)$
        $_offerInvitationSentEntryIfoneSignatory()$
    $else$
        $_offerInvitationSentEntryIfElse()$
    $endif$ 
    
    $ip$
###
orderInvitationSentEntry=
    $authorfstname$ $authorsndname$ $_orderInvitationSentEntry()$
    $if(oneSignatory)$
        $_orderInvitationSentEntryIfoneSignatory()$
    $else$
        $_orderInvitationSentEntryIfElse()$
    $endif$ 
    
    $ip$
###
contractSeenHistEntry=
    $personname$ $_contractSeenHistEntry()$ $seenip$
###
offerSeenHistEntry=
    $personname$ $_offerSeenHistEntry()$ $seenip$
####
orderSeenHistEntry=
    $personname$ $_orderSeenHistEntry()$ $seenip$
####
contractSignHistEntry=
    $personname$ $_contractSignHistEntry()$ $signMethod()$ $signip$
####
offerSignHistEntry=
    $personname$ $_offerSignHistEntry()$ $signMethod()$ $signip$
###
orderSignHistEntry=
    $personname$ $_orderSignHistEntry()$ $signMethod()$ $signip$
###
signMethod=
        $if(!provider)$
          $if(email)$
            $_signMethodEmail()$
          $elseif(pad)$
            $_signMethodPad()$
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
