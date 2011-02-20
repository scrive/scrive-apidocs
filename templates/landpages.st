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
    <p>Du kan nu spara dokumentet på SkrivaPå, det kostar ingenting att lagra dokumentet
       och just nu bjuder vi på fri användning av tjänsten i 2 månader!
       Klicka nedan så är det klart.
      <form action="$LinkLandpageSaved()$" method="get">
        <input class="button" type="submit" value="Skapa konto" />
      </form>
    </p>
####
landpageSignedViewClosed=
    <div class="centerdivnarrow">
      <p class="headline">Du har undertecknat dokumentet $documenttitle$</p>
      <p>$partyListString:noescape()$ har nu undertecknat och dokumentet är bindande.</p>
         $willCreateAccountForYou:noescape()$
    </div>
#####
landpageSignedViewNotClosed=
    <div class="centerdivnarrow">
      <p class="headline">Du har undertecknat dokumentet $documenttitle$</p>

      <p>$partyUnsignedListString:noescape()$ har ännu inte undertecknat. När alla undertecknat blir
         dokumentet bindande och en kopia skickas till din e-post.</p>

         $willCreateAccountForYou:noescape()$
    </div>
###
