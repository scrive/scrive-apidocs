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
      , docPrefix="Offert nummer"
      , signedText="Offert accepterad"
      , partnerText="Mottagare"
      , secretaryText="Avsändare"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och bekräftar att offert nummer $documentid$ har skickats"
        , "av avsändaren och accepterats av mottagaren. Kursiverad information är verifierad genom vår tjänst."
        , "Du kan kontrollera offerten mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }
#####
ordersealingtexts= 
SealingTexts
    {   verificationTitle="Orderbekräftelse"
      , docPrefix="Orderbekräftelse nummer"
      , signedText="Ordern bekräftad"
      , partnerText="Mottagare"
      , secretaryText="Avsändare"
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
contractLastHistEntry=
    Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande.
#####
offerLastHistEntry=
    Samligta parter har bekräftat offerten.
####
orderLastHistEntry=
    Samligta parter har bekräftat ordern.
####
contractInvitationSentEntry=
    $authorfstname$ $authorsndname$ skickar en inbjudan att underteckna till
    $if(oneSignatory)$
        parterna
    $else$
        parten
    $endif$ 
    
    $ip$
###
offerInvitationSentEntry=
    $authorfstname$ $authorsndname$ skickar offerten till $personname$ $ip$
###
orderInvitationSentEntry=
    $authorfstname$ $authorsndname$ skickar orderbekräftelsen till
    $if(oneSignatory)$
        parterna
    $else$
        parten
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
    $personname$ undertecknar dokumentet online med 
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
    $signip$
####
offerSignHistEntry=
    $personname$ accepterar offerten online $signip$
###
orderSignHistEntry=
    $personname$ bekräftar ordern online med 
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
    $signip$
###
