#####
sealingtexts= 
$if(contract)$
SealingTexts
    {   verificationTitle="Verifikat"
      , docPrefix="Dok.nr."
      , signedText="Undertecknat:"
      , partnerText="Parter"
      , secretaryText="Ej undertecknande part"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och styrker att dokument nummer $documenttitle$ har undertecknats"
        , "av parterna och är juridiskt bindande. Kursiverad information är säkert verifierad genom vår tjänst."
        , "Kontrollera dokumentet mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }
$elseif(offer)$
SealingTexts
    {   verificationTitle="Verifikat"
      , docPrefix="Dok.nr."
      , signedText="Undertecknat:"
      , partnerText="Parter"
      , secretaryText="Ej undertecknande part"
      , orgNumberText="Org.nr. "
      , eventsText="Registrerade händelser"
      , dateText="Datum"
      , historyText="Händelser"
      , verificationFooter=[
          "Detta verifikat är utfärdat av SkrivaPå CM AB och styrker att dokument nummer $documenttitle$ har undertecknats"
        , "av parterna och är juridiskt bindande. Kursiverad information är säkert verifierad genom vår tjänst."
        , "Kontrollera dokumentet mot vår databas genom följande länk: $hostpart$/d/$documentid$."]
    }
$endif$
#####
lastHistEntry=
    $if(contract)$
        Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande.
    $elseif(offer)$
        Samligta parter har bekräftat offerten.
    $endif$
####
invitationSentEntry=
    $authorfstname$ $authorsndname$ 
    $if(contract)$
        skickar en inbjudan att underteckna till
    $elseif(offer)$
        skickar en offert att bekräfta till
    $endif$ 
    $if(oneSignatory)$
        parterna
    $else$
        parten$endif$ 
    $ip$
###
seenHistEntry=
    $personname$ 
    $if(contract)$
        granskar dokumentet
    $elseif(offer)$
        granskar offerten
    $endif$ 
        online $seenip$
####
signHistEntry=
    $personname$ 
    $if(contract)$
        undertecknar dokumentet
    $elseif(offer)$
        bekräftar offerten
    $endif$ 

        online med 

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
signHistEntryMethod=
   
###



