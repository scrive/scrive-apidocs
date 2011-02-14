###
landpageSignInviteView=
    <div class="centerdivnarrow">
     <p class="headline">Du har undertecknat dokumentet <strong>$documenttitle$</strong>!</p>
     <p>En inbjudan att underteckna har nu skickats 
        till $partyListButAuthor:noescape()$.
      </p>
      <p><a class="button" href="/">Skapa ett nytt dokument</a></p>
    </div>
######
landpageSendInviteView=
    <div class="centerdivnarrow">
      <p class="headline">Dokumentet <strong>$documenttitle$</strong> skickat!</p>
      <p>En inbjudan att underteckna har nu skickats
        till $partyListButAuthor:noescape()$.
      </p>
      <p><a class="button" href="/">Skapa ett nytt dokument</a></p>
    </div>
######
landpageLoginForSaveView=
    <div class="centerdivnarrow">
     <a class="headline">Konto har skapats</a>
     <p>Ditt konto har skapats. En aktiveringslänk har skickats till din inkorg.</p>
    </div>
#####
landpageRejectedView=
    <div class="centerdivnarrow">
      <p class="headline">Du har avvisat dokumentet <strong>$documenttitle$</strong>.</p>
      <p>Ett meddelande har skickats till $partyList:noescape()$.</p>
      <p><a href="/">Tillbaka till förstasidan</a></p>
    </div>
#####
willCreateAccountForYouHasAccount=
    <p>Dokumentet har sparats till ditt konto.
      $loginBox()$
    </p>
#####
willCreateAccountForYouNoAccount=
    <p>Du kan nu spara dokumentet på SkrivaPå, då är ditt dokument säkert lagrat och dessutom
       kan du även i framtiden verifiera avtalet mot vår databas. Detta kostar ingenting.
      <form action="$LinkLandpageSaved()$" method="get">
        <input class="button" type="submit" value="Skapa konto">
      </form>
    </p>
####
landpageSignedViewClosed=
    <div class="centerdivnarrow">
      <p class="headline">Dokumentet är färdigställt</p>
      <p>Du har undertecknat dokumentet <strong>$documenttitle$</strong>. Således har
         $partyListString:noescape()$ undertecknat dokumentet och avtalet är nu juridiskt bindande.</p>
         $willCreateAccountForYou:noescape()$
    </div>
#####
landpageSignedViewNotClosed=
    <div class="centerdivnarrow">
      <p class="headline">Du har undertecknat</p>
      <p>Du har nu undertecknat dokumentet <strong>$documenttitle$</strong>.
         $partyUnsignedListString:noescape()$ har ännu inte undertecknat
         dokumentet. När alla undertecknat blir avtalet juridiskt bindande och
         en kopia av det färdigställda dokumentet skickats då till din e-post.</p>
         $willCreateAccountForYou:noescape()$
    </div>
###
