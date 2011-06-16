#####
contractsealingtexts= 
SealingTexts
    {   verificationTitle="Verifikat"
      , docPrefix="Dok.nr."
      , signedText="Undertecknat"
      , partnerText="Parter"
      , secretaryText="Ej undertecknande part"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och styrker att dokument nummer $documentid$ har undertecknats"
        , "av parterna och är juridiskt bindande. Kursiverad information är säkert verifierad genom vår tjänst."
        , "Kontrollera dokumentet mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }
#####
offersealingtexts=
SealingTexts
    {   verificationTitle="Acceptverifikat"
      , docPrefix="Offertnummer"
      , signedText="Offert accepterad"
      , partnerText="Mottagare"
      , secretaryText="Avsändare"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och bekräftar att offertnummer $documentid$ har accepterats"
        , "av parterna och är juridiskt bindande. Kursiverad information är verifierad genom vår tjänst."
        , "Du kan kontrollera offerten mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }
#####
ordersealingtexts= 
SealingTexts
    {   verificationTitle="Orderbekräftelse"
      , docPrefix="Orderbekräftelsenummer"
      , signedText="Ordern bekräftad"
      , partnerText="Mottagare"
      , secretaryText="Avsändare"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och styrker att orderbekräftelsenummer $documentid$ har bekräftats"
        , "av parterna och är juridiskt bindande. Kursiverad information är säkert verifierad genom vår tjänst."
        , "Kontrollera orderbekräftelsen mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }
#####
contractLastHistEntry=
    Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande.
#####
offerLastHistEntry=
    Samligta parter har accepterat offerten.
####
orderLastHistEntry=
    Samligta parter har bekräftat ordern.
####
contractInvitationSentEntry=
    $authorfstname$ $authorsndname$ skickar en inbjudan att underteckna till
    $if(oneSignatory)$
        parten
    $else$
        parterna
    $endif$ 
    
    $ip$
###
offerInvitationSentEntry=
    $authorfstname$ $authorsndname$ skickar offerten till
    $if(oneSignatory)$
        parten
    $else$
        parterna
    $endif$ 
    
    $ip$
###
orderInvitationSentEntry=
    $authorfstname$ $authorsndname$ skickar orderbekräftelsen till
    $if(oneSignatory)$
        parten
    $else$
        parterna
    $endif$ 
    
    $ip$
###
contractSeenHistEntry=
    $personname$ granskar dokumentet online $seenip$
###
offerSeenHistEntry=
    $personname$ granskar offerten online $seenip$
####
orderSeenHistEntry=
    $personname$ granskar orderbekräftelsen online $seenip$
####
contractSignHistEntry=
    $personname$ undertecknar dokumentet online med $signMethod()$ $signip$
####
offerSignHistEntry=
    $personname$ accepterar offerten med $signMethod()$ $signip$
###
orderSignHistEntry=
    $personname$ bekräftar ordern online med $signMethod()$ $signip$
###
signMethod=
        $if(!provider)$
            e-post
        $elseif(bankid)$
            BankID
        $elseif(norder)$
            Nordea e-legitimation
        $elseif(telia)$
            Telia e-legitimation
        $else$
            e-legitimation
        $endif$
###
