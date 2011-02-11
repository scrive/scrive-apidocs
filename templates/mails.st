######
mailCancelDocumentByAuthorTitle=Återkallelsenotis: $documenttitle$
####
mailCancelDocumentByAuthorStandardHeader=
    <p>Hej, </p>
######
mailCancelDocumentByAuthorContent=
    $header:noescape()$
    <p>
      <strong>$creatorname$</strong> har valt att återkalla sin inbjudan att underteckna dokument $documenttitle$.
      Därmed är avtalsprocessen avbruten och du kan inte längre underteckna dokumentet.
    </p>
    $footer:noescape()$
#####
mailDocumentAwaitingForAuthorTitle=Vänligen underteckna $documenttitle$
######
mailDocumentAwaitingForAuthorContent=
    <span>
      <p>Hej <strong>$authorname$</strong>,</p>
      <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats av $signatories$. För att avtalet skall färdigställas krävs din underskrift.
      </p>
      <a href="$documentlink$">$documenttitle$</a>
      $poweredBySkrivaPaPara()$
    </span>
#####
mailDocumentClosedForAuthorTitle=Bekräftelse: $documenttitle$
#####
mailDocumentClosedForAuthorContent=
    <span>
      <p>Hej <strong>$authorname$</strong>,</p>
      <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats
        av $partylist:noescape()$. Avtalet är nu bindande.</p>
      <p>Det färdigställda dokumentet bifogas med detta mail.</p>
      $poweredBySkrivaPaPara()$
    </span>
#####
mailDocumentClosedForSignatoriesTitle=Bekräftelse: $documenttitle$
#####
mailDocumentClosedForSignatoriesContent=
    <span>
      <p>Hej <strong>$personname$</strong>,</p>
      <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats
        av $partylist:noescape()$. Avtalet är nu bindande.</p>
      <p>Det färdigställda dokumentet bifogas med detta mail.</p>
      $poweredBySkrivaPaPara()$
    </span>
####
mailRejectMailTitle=Avvisat: $documenttitle$
####
mailRejectMailContent=
    <span>
      $if(customMessage)$
       $customMessage:noescape()$ 
      $else$
      <div class="editable" name="customtext">
      <p>Hej,</p>
      </div>
      $endif$
      <p><strong>$rejectorName$</strong> har avvisat att underteckna dokumentet <strong>$documenttitle$</strong>. Avtalsprocessen är därmed avbruten.
      </p>
     </span>
###
mailInvitationToSignTitle=Inbjudan från $creatorname$ till $documenttitle$
###
mailInvitationToSignContent=
    $header:noescape()$
    <span>
      <p><i>Översiktlig information</i><br>
        Parter: $partnersinfo:noescape()$<br>
        $whohadsignedinfo:noescape()$<br>
        $timetosigninfo:noescape()$
      </p>
      <p><i>Så här går du vidare</i><br>
        1. Klicka på länken <br>
        <a href="$link$">$link$</a> <br>
        <br>
        2. Granska online<br>
        3. Underteckna (eller avvisa)<br>
        4. Färdig
      </p>
      <p>
	<i>Detta mail får inte vidarebefordras.</i>
      </p>
    </span>
    $footer:noescape()$
###
mailInvitationToSignDefaultHeader=
    <p>Hej $personname$,
      <br><br>
      <strong>$creatorname$</strong> har bjudit in dig att underteckna dokumentet <strong>$documenttitle$</strong> online via tjänsten SkrivaPå.
    </p>
####
remindMailSignedTitle=$documenttitle$
###
remindMailSignedStandardHeader=
    <span>
      <p>Hej <strong>$personname$</strong></p>
      <p><strong>$author$</strong> har begärt att vi ska skicka dokumentet <strong>$documenttitle$</strong>
        till dig som du har undertecknat via tjänsten SkrivaPå. Dokumentet bifogas med detta mail.</p>
    </span>
####
remindMailNotSignedTitle=Påminnelse att underteckna  $documenttitle$
########
remindMailNotSignedStandardHeader=
    <span>
      <p>Hej <strong>$personname$</strong></p>
      <p><strong>$author$</strong> vill påminna dig om att du inte undertecknat dokument <strong>$documenttitle$</strong> ännu.</p>
    </span>
#####
remindMailNotSignedContent=
    $header:noescape()$
    <span>
      <p><i>Översiktlig information</i><br>
        Parter: $partnersinfo:noescape()$<br>
        $whohadsignedinfo:noescape()$<br>
        $timetosigninfo:noescape()$
      </p>
      <p><i>Så här går du vidare</i><br>
        1. Klicka på länken<br>
        <a href="$link$">$link$</a><br>
        2. Granska online<br>
        3. Underteckna (eller avvisa)<br>
        4. Färdig
      </p>
      <p>
	<i>Detta mail får inte vidarebefordras.</i>
      </p>
    </span>
    $footer:noescape()$

###
signupLinkEmailPart=
    <p>Användarnamn:<br>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br>
    <br>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a><p>

    $poweredBySkrivaPaPara()$

####
newUserMailTitle=Nytt konto
####
newUserMailContent=
    <p>Hej <strong>$personname$</strong>,</p>
    <p>Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har skapat ett konto hos oss.
       Vi hoppas att du kommer att bli nöjd med våra tjänster.
       Tveka inte att höra av dig med feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>

    <p>Användarnamn:<br>
    $email$</p>
    
    <p>Du aktiverar ditt konto genom att klicka på länken nedan. Länken är giltig i 12 timmar.<br>
    <br>
    <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a><p>

    $poweredBySkrivaPaPara()$
####
passwordChangeMailTitle=Nytt lösenord
####
passwordChangeMailContent=
    <p>Hej $personname$,</p>
    <p>Du har begärt ett nytt lösenord. Vänligen klicka på länken nedan för att välja ett nytt lösenord. Länken är giltig i 12 timmar.<br>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a>
    </p>
    $poweredBySkrivaPaPara()$
####
inviteSubaccountMailTitle=Inbjudan från $supervisorname$ till underkonto
####
inviteSubaccountMailContent=
    <p>Hej <strong>$personname$</strong>,</p>

    <p><strong>$supervisorname$</strong> har bjudit in dig till ett underkonto på SkrivaPå genom vilket du kan 
    skriva avtal för <strong>$companyname$</strong>.
    Observera att detta konto inte är avsett för privat bruk.</p>

    <p>Användarnamn:<br>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a><p>

    $poweredBySkrivaPaPara()$
####
mailViralInviteTitle=Inbjudan från $invitername$
###
mailViralInviteContent=
    <p>Hej,</p>

    <p>$invitername$ har bjudit in dig till ett konto på tjänsten SkrivaPå. 
    SkrivaPå är en tjänst för att skriva på och arkivera dokument elektroniskt. 
    Spara pengar, frustration och miljö och använd SkrivaPå istället. 
    Du kan spara mellan 50-250kr/dokument!</p>

    <p>Användarnamn:<br>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a><p>

    $poweredBySkrivaPaPara()$

####
mailNewAccountCreatedByAdminTitle=Nytt konto

###
mailNewAccountCreatedByAdminContent=
    <p>Hej <strong>$personname$</strong>,</p>

    <p>$creatorname$ har bjudit in dig till ett konto på tjänsten SkrivaPå. 
    SkrivaPå är en tjänst för att skriva på och arkivera dokument elektroniskt. 
    Spara pengar, frustration och miljö och använd SkrivaPå istället. 
    Du kan spara mellan 50-250kr/dokument!</p>

    <p>Användarnamn:<br>
    $email$</p>
    
    <p>Vänligen klicka på länken för att aktivera kontot:<br>
    <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a><p>

    $poweredBySkrivaPaPara()$
####
invitationMailUndeliveredTitle=Felaktig e-post
###
invitationMailUndeliveredContent=
    <span>
      <p>Hej $authorname$,</p>
      <p>Adressen $email$ existerar inte och därför kunde vi inte leverera din inbjudan att underteckna via e-post.
	Processen har därmed avbrutits. Gör så här om du vill börja om:</p>
      <p>1. Kontrollera vilken som är mottagarens korrekta adress<br>
	2. Klicka på dokumentet i ditt arkiv eller på följande länk<br>
	<a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a><br>
        <br>
        3. Klicka "Starta om"<br>
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
    <p>Du har begärt att ändra ditt lösenord på SkrivaPå. Vänligen klicka på följande länk för att gå vidare. Länken är giltig i 12 timmar.<br>
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
