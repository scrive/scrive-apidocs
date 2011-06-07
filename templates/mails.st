######
mailCancelDocumentByAuthorTitle=Återkallelsenotis: $documenttitle$
####
mailCancelDocumentByAuthorStandardHeader=
    $if(contract)$
        <p>Hej, </p>
    $elseif(offer)$
        <p>Hej, $partylist$ </p>
    $endif$

######
mailCancelDocumentByAuthorContent=
    $header:noescape()$
    <p>
      $if(contract)$
      <strong>$creatorname$</strong> har valt att återkalla sin inbjudan att underteckna dokument $documenttitle$.
      Därmed är avtalsprocessen avbruten och du kan inte längre underteckna dokumentet.
      $elseif(offer)$
       <strong>$creatorname$</strong>   har valt att återkalla offerten  $documenttitle$.
      $endif$
    </p>
    $footer:noescape()$
#####
mailDocumentAwaitingForAuthorTitle=Vänligen underteckna $documenttitle$
######
mailDocumentAwaitingForAuthorContent=
    <p>Hej <strong>$authorname$</strong>,</p>
    <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats av $partylist:noescape()$.
      Därmed är det bara du kvar innan dokumentet är färdigställt.</p>
    <p>För att färdigställa dokumentet gör följande:<br/>
      1. Klicka på länken<br/>
      <a href="$documentlink$">$documenttitle$</a><br/>
      <br/>
      2. Granska uppgifterna<br/>
      3. Underteckna För att avtalet skall färdigställas krävs din underskrift.</p>
    $poweredBySkrivaPaPara()$
#####
mailDocumentClosedForAuthorTitle=Bekräftelse: $documenttitle$
#####
mailDocumentClosedForAuthorContent=
    <span>
      <p>Hej <strong>$authorname$</strong>,</p>
       $if(contract)$
       <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats
         av $partylist:noescape()$. Avtalet är nu bindande.</p>
       <p>Det färdigställda dokumentet bifogas med detta mail.</p>
       $elseif(offer)$
         <p>Offerten <strong>$documenttitle$</strong> har accepterats av $partylist:noescape()$ och SkrivaPå har utfärdat ett verifikat för accepten. 
         Bifogat med detta mail finner du en PDF med offerten och verifikatet.</p>
       $endif$
      $poweredBySkrivaPaPara()$
    </span>
#####
mailDocumentClosedForSignatoriesTitle=Bekräftelse: $documenttitle$
#####
mailDocumentClosedForSignatoriesContent=
    <span>
      <p>Hej <strong>$personname$</strong>,</p>
      $if(contract)$
       <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats
         av $partylist:noescape()$. Avtalet är nu bindande.</p>
       <p>Det färdigställda dokumentet bifogas med detta mail.</p>
      $elseif(offer)$
       <p>Offerten <strong>$documenttitle$</strong> har accepterats av $partylist:noescape()$ och SkrivaPå har utfärdat ett verifikat för accepten. </p>
       <p>Bifogat med detta mail finner du en PDF med offerten och verifikatet.</p>

      $endif$ 
      $poweredBySkrivaPaPara()$
    </span>
####
mailRejectMailTitle=Avvisat: $documenttitle$
####
mailRejectMailContent=
      $if(customMessage)$
       $customMessage:noescape()$ 
      $else$
      <div class="editable" name="customtext">
      <p>Hej,</p>
      </div>
      $endif$
      $if(contract)$
      <p>
        <strong>$rejectorName$</strong> har avvisat att underteckna dokumentet <strong>$documenttitle$</strong>. Avtalsprocessen är därmed avbruten.
      </p>
      $elseif(offer)$
      <p>
        <strong>$rejectorName$</strong> har avvisat offerten <strong>$documenttitle$</strong>.
      </p>
      $endif$
      $poweredBySkrivaPaPara()$

###
mailInvitationToViewTitle=Inbjudan från $creatorname$ till $documenttitle$
###
mailInvitationToSignTitle=Inbjudan från $creatorname$ till $documenttitle$
###
mailInvitationToSignContent=
    $header:noescape()$
    $if(!offer)$
      <p>
       <i>Översiktlig information</i><br/>
       Parter: $partnersinfo:noescape()$<br/>
       $whohadsignedinfo:noescape()$<br/>
       Avsändare: <strong>$creatorname$</strong><br/>
       $timetosigninfo:noescape()$
      </p>
    $endif$
    $if(issignatory)$
    <p>
      $if(!offer)$
        <i>Så här går du vidare</i><br/>
      $endif$
      1. Klicka på länken <br/>
      <a href="$link$">$link$</a><br/>
      2. Granska online<br/>
      $if(offer)$ 
      3. Acceptera offert (eller avvisa)<br/> 
      $else$ 
      3. Underteckna (eller avvisa)<br/> 
      $endif$
      4. Färdig
    </p>
    $else$
    Du har endast behörighet att se detta dokument
    Klicka på länken nedan för att se dokumentet:
    <br/> 
    <a href="$link$">$link$</a> <br/>
    <br/>
    $endif$
    $footer:noescape()$
    $legalparagraph()$
###
mailInvitationToSignDefaultHeader=
    <p>Hej $personname$,
      <br/><br/>
      $if(contract)$
       <strong>$creatorname$</strong> har bjudit in dig att underteckna dokumentet <strong>$documenttitle$</strong> online via tjänsten SkrivaPå.
      $elseif(offer)$
      <strong>$creatorname$</strong> har skickat offerten <strong>$documenttitle$</strong>  till dig och önskar din accept.
      Acceptera offerten genom att följa instruktionerna nedan.
      $endif$
    </p>
####
mailInvitationToViewDefaultHeader=
    <p>Hej $personname$,
      <br/><br/>
      <strong>$creatorname$</strong> har bjudit in dig att se dokumentet <strong>$documenttitle$</strong> online via tjänsten SkrivaPå.
    </p>
####
remindMailSignedTitle=$documenttitle$
###
remindMailSignedStandardHeader=
    <span>
      <p>Hej <strong>$personname$</strong></p>
      $if(contract)$
      <p><strong>$author$</strong> har begärt att vi ska skicka dokumentet <strong>$documenttitle$</strong>
        till dig. Dokumentet har undertecknats av dig via tjänsten SkrivaPå. Dokumentet bifogas med detta mail.</p>
      $elseif(offer)$
      <p><strong>$author$</strong> har  har begärt att vi ska skicka offerten  <strong>$documenttitle$</strong>
        till dig. Offerten har accepterats av dig via tjänsten SkrivaPå. Den accepterade offerten bifogas med detta mail.</p>
      $endif$
    </span>
####
remindMailNotSignedTitle=Påminnelse att underteckna  $documenttitle$
########
remindMailNotSignedStandardHeader=
    <span>
      <p>Hej <strong>$personname$</strong></p>
      $if(contract)$ 
      <p><strong>$author$</strong> vill påminna dig om att du inte undertecknat dokument <strong>$documenttitle$</strong> ännu.</p>
      $elseif(offer)$
      <p><strong>$author$</strong> vill påminna dig om att du inte accepterat offerten <strong>$documenttitle$</strong> ännu.
        Acceptera offerten genom att följa instruktionerna nedan.
      </p>

      $endif$
    </span>
#####
remindMailNotSignedContent=
    $header:noescape()$
    <span>
      $if(!offer)$ 
      <p><i>Översiktlig information</i><br/>
        Parter: $partnersinfo:noescape()$<br/>
        $whohadsignedinfo:noescape()$<br/> 
        $timetosigninfo:noescape()$
      </p>
      $endif$
      <p>
        $if(!offer)$ <i>Så här går du vidare</i><br/> $endif$
        1. Klicka på länken<br/>
        <a href="$link$">$link$</a><br/>
        2. Granska online<br/>
        $if(offer)$ 
           3.  Acceptera offert (eller avvisa)<br/> 
        $else$ 
           3. Underteckna (eller avvisa)<br/> 
        $endif$
        4. Färdig
      </p>
    </span>
    $footer:noescape()$
    $legalparagraph()$

###
signupLinkEmailPart=
    <p>Användarnamn:<br/>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br/>
    <br/>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$

####
newUserMailTitle=Nytt konto
####
newUserMailContent=
<p>Hej,</p>
<p>

  Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har
  skapat ett konto hos oss. När du aktiverat ditt konto kommer du
  kunna använda tjänsten med full funktionalitet med kontoplanen
  Business i 1 månad utan kostnad. Väljer du att inte fortsätta med en
  betald kontoplan efter testperioden behåller du givetvis tillgång
  till samtliga dina dokument på SkrivaPå och kan fortsätta använda
  tjänsten genom vår fria kontoplan Free.

</p>
<p> 

  Vi hoppas att du kommer att bli nöjd med våra tjänster. Tveka inte
  att höra av dig med feedback eller bara en enkel hälsning. Din åsikt
  är värdefull.

</p>
<p>

  Användarnamn:<br/>
  $email$

</p>

<p>

  Du aktiverar ditt konto genom att klicka på länken nedan. Länken är
  giltig i 12 timmar.<br/>
  
  <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a>

</p>

$poweredBySkrivaPaPara()$
####
inviteSubaccountMailTitle=Inbjudan från $supervisorname$ till underkonto
####
inviteSubaccountMailContent=
    <p>Hej <strong>$personname$</strong>,</p>

    <p><strong>$supervisorname$</strong> har bjudit in dig till ett
    underkonto på SkrivaPå genom vilket du kan skriva avtal
    för organisationen <strong>$companyname$</strong>. Observera att detta konto inte
    är avsett för privat bruk då samtliga dokument i kontot
    tillhör organisationen

    <p>Användarnamn:<br/>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br/>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$
####
mailAccountBySigningContractReminderTitle=Du har inte säkrat ditt original
###
mailAccountBySigningContractReminderContent=
<p>Hej <strong>$personname$</strong>,</p>

<p>Du har inte bekräftat att du vill spara ditt dokument "$documenttitle$" hos SkrivaPå. SkrivaPås system kommer att radera dokumentet automatiskt inom 72h. Det kostar ingenting att spara dokumentet.</p>

<p>Gör så här:<br>
1. Klicka på länken<br>
<a href="$ctxhostpart$$activationlink$">$ctxhostpart$$activationlink$</a><br>
2. Definiera lösenord och acceptera villkoren<br>
3. Färdig</p>

$poweredBySkrivaPaPara()$
####
mailAccountBySigningOfferReminderTitle=Du har inte sparat ditt original
###
mailAccountBySigningOfferReminderContent=
<p>Hej <strong>$personname$</strong>,</p>

<p>Du har inte bekräftat att du vill spara din offert "$documenttitle$" hos SkrivaPå. SkrivaPås system kommer att radera offerten automatiskt inom 72h. Det kostar ingenting att spara offerten.</p>

<p>Gör så här:<br>
1. Klicka på länken<br>
<a href="$ctxhostpart$$activationlink$">$ctxhostpart$$activationlink$</a><br>
2. Definiera lösenord och acceptera villkoren<br>
3. Färdig</p>

$poweredBySkrivaPaPara()$
###
mailViralInviteTitle=Inbjudan från $invitername$
###
mailViralInviteContent=
    <p>Hej,</p>

    <p>$invitername$ har bjudit dig på två månaders fri användning av tjänsten SkrivaPå.
    Det betyder nog att han/hon gillar vår tjänst och tror du också skulle uppskatta den också.</p>

    <p>SkrivaPå är en tjänst för att skriva på och arkivera dokument elektroniskt.
    Spara pengar, frustration och miljö och använd SkrivaPå istället. 
    Du kan spara mellan 50-250kr/dokument!</p>

    <p>Användarnamn:<br/>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br/>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$

####
mailNewAccountCreatedByAdminTitle=Nytt konto
####
mailNewAccountCreatedByAdminContentHeader=
 
    <p>Hej <strong>$personname$</strong>,</p>

    <p>$creatorname$ har bjudit in dig till ett konto på tjänsten SkrivaPå. 
    SkrivaPå är en tjänst för att skriva på och arkivera dokument elektroniskt. 
    Spara pengar, frustration och miljö och använd SkrivaPå istället. 
    Du kan spara mellan 50-250kr/dokument!</p>

###
mailNewAccountCreatedByAdminContent=
   $if(custommessage)$
    $custommessage:noescape()$
    $else$
    $mailNewAccountCreatedByAdminContentHeader()$
    $endif$
   
    <p>Användarnamn:<br>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br/>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>

    $poweredBySkrivaPaPara()$
####
invitationMailDeferredTitle=Problem med leverans
###
invitationMailDeferredContent=
    <span>
      <p>Hej $authorname$,</p>
      <p>Vi skickar detta meddelande för att meddela att vårt system har identifierat ett problem med mottagarens e-postserver. Antingen finns inte e-postadressen eller så ligger mottagarens e-postserver nere för tillfället. Vänligen kontrollera att mottagarens e-post är korrekt.</p>
      <p>Vi fortsätter försöka att nå mottagarens server och meddelar inom kort hur det gått. Klicka på länken nedan om du vill stoppa våra försök eller om du vill ändra e-post adressen.<br />
      <a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a></p>
      $poweredBySkrivaPaPara()$
    </span>
###
invitationMailDeliveredAfterDeferredTitle=Lyckosam leverans
###
invitationMailDeliveredAfterDeferredContent=
    <span>
      <p>Hej $authorname$,</p>
      <p>Vi meddelade dig tidigare om att vi hade problem att nå mottagarens server. Efter upprepade försök har vår server lyckats leverera inbjudan till dokumentet <strong>$documenttitle$</strong> till $email$.</p>
      $poweredBySkrivaPaPara()$
    </span>
###
invitationMailUndeliveredTitle=Felaktig e-post
###
invitationMailUndeliveredContent=
    <span>
      <p>Hej $authorname$,</p>
       <p>Vi har inte kunnat leverera din inbjudan att underteckna dokumentet <strong>$documenttitle$</strong>. Det beror antingen på problem med mottagarens e-post server eller mer troligt på att adressen $email$ inte existerar.
       Processen har därmed avbrutits. Gör så här om du vill börja om:</p>
       <p>1. Kontrollera vilken som är mottagarens korrekta adress<br/>
       2. Klicka på dokumentet i ditt arkiv eller på följande länk<br/>
       <a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a><br/>
       <br/>
       3. Klicka "Starta om"<br/>
       4. Skriv in rätt e-postadress och skicka igen
      </p>
      $poweredBySkrivaPaPara()$
    </span>
####
mailDocumentErrorTitle=SkrivaPå could no handle $documenttitle$
###
mailDocumentErrorContent=
    <p>Hej,</p>
    <p>We are sorry, but we could not handle document <strong>$documenttitle$</strong>.
      There are probably very good reasons for this, but they might be hard to explain.</p>
    <p>Our technicians were notified about this issue. We will (probably) contact you when fix is available.</p>
    <p>Processen har därmed avbrutits.</p>
    <p>We are sorry for the inconvenience</p>
    $poweredBySkrivaPaPara()$
###
passwordChangeLinkMailTitle=Nytt lösenord
###
passwordChangeLinkMailContent=
    <p>Hej $personname$,</p>
    <p>Du har begärt att ändra ditt lösenord på SkrivaPå. Vänligen klicka på följande länk för att gå vidare. Länken är giltig i 12 timmar.<br/>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a></p>
    $poweredBySkrivaPaPara()$
###
###
####UTILS
timetosigninfo=
    <span>Undertecknas senast: <strong>$time$</strong>.</span>
###
updateinglistwithauthor=
    <span>
      <span class="partylistupdate"></span>
    </span>
####
noonehadsignedtext=
    Ingen har undertecknat dokumentet ännu
####
whohadsignedinfoformail=
    $if(signedlist)$
    Har undertecknat: $signedlist:noescape()$
    $else$
    Har undertecknat: $noonehadsignedtext()$
    $endif$
####
whohadsignedinfoforpreview=
    <span class="authornamewhennotsecretary" alt="$noonehadsignedtext()$" okprefix="Har undertecknat:"></span>
####
####
legalparagraph=
<p style="width:90%;font-size:8pt;font-style:italic;"> 
   Detta e-postmeddelande innehåller konfidentiell information och får inte vidarebefordras.
   Avslöjande, kopiering, distribution eller annan behandling av informationen i 
   meddelandet är förbjuden och kan vara olagligt. Om du felaktigt mottagit detta meddelande 
   ber vi dig omedelbart informera avsändaren samt att därefter radera meddelandet. 
</p>
<BR/>
####
mailMismatchSignatoryTitle=Problem med matchning av personuppgifter
####
mailMismatchSignatoryContent=
<p>Hej $signame$</p>

<p>Vi beklagar, men någon av de uppgifter <a href="mailto:$authoremail$">$authorname$</a> har angivit i <a href="$doclink$">$documenttitle$</a> matchar inte personuppgifterna hos $badname$s e-legitmation.</p>

<p>Avtalsprocessen för <a href="$doclink$">$documenttitle$</a> har därför avbrutits. Var vänliga invänta en ny inbjudan eller kontakta <a href="mailto:$authoremail$">$authorname$</a>.</p>

$if(messages)$
<p><strong>Felrapport:</strong></p>
$messages:noescape()$
$endif$

<p>
Med Vänliga Hälsningar
</p>
<p>
SkrivaPå
</p>
####
mailMismatchAuthorTitle=Felaktiga personuppgifter
####
mailMismatchAuthorContent=
<p>Hej $authorname$</p>

<p>Vi beklagar, de uppgifter du angivet i <a href="$doclink$">$documenttitle$</a> matchar inte personuppgifterna hos <a href="mailto:$bademail$">$badname$</a>s e-legitimation.</p>

<p>Avtalsprocessen för <a href="$doclink$">$documenttitle$</a> har därför avbrutits. Du kan välja att starta om avtalsprocessen och skicka en ny inbjudan till dina motparter, ett e-post meddelande har även skickats ut till dina motparter med information om att avtalsprocessen har avbrutits.</p>
 
<p><strong>Felrapport:</strong></p>

$messages:noescape()$

<p>Med vänliga hälsningar</p>
<p>SkrivaPå</p>
####
mailInviteUserAsSubaccountTitle=Förfrågan att göra till underkonto
###
mailInviteUserAsSubaccountContent=
  <p>Hej $invited.fullnameOrEmail$,</p>
  <p>Användaren $supervisor.fullnamePlusEmail$ har
 begärt att få bli huvudkonto till ditt konto. Om du accepterar blir
 ditt konto ett underkonto till avsändaren. Du kommer fortfarande att kunna se och administrera dina avtal, dock tar huvudkontot över ägarskapet över
 samtliga dokument i ditt konto. </p>

 <p>Vänligen iaktta försiktighet innan du accepterar. Särskilt om du
  har privata dokument i kontot.</p>

 <p>Så här går du vidare</p>
 <ul>
 <li>Kontrollera att du inte har några privata dokument i kontoarkiven</li>
 <li>Spara ned samtliga privata dokument till din dator</li>
 <li>Klicka på länken nedan</li>
 <li>Läs meddelandet online</li>
 <li>Acceptera</li>
 <li>Ditt konto är nu underkonto</li>

 <p><a href="$hostpart$/account/bsa/$supervisor.id$">Detta är länken nedan</a></p>
  $poweredBySkrivaPaPara()$
####
####
mailSubaccountAcceptedTitle=Kontoförfrågan
###
mailSubaccountAcceptedContent=
  <p>Hej $user.fullnameOrEmail$,</p>
  <p>$invited.fullnameOrEmail$ har accepterat att bli ett underkonto.</p>
  $poweredBySkrivaPaPara()$
####

