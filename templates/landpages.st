### 
landpageSignInviteView=
    <div class="centerdivnarrow">
     <p class="headline">Dokumentet <strong>$documenttitle$</strong> undertecknat!</p>
     <p>En inbjudan att underteckna har nu skickats 
        till $partyListButAuthor:noescape()$.
     </p>
     <p><a class="button" href="/">Skapa ett nytt avtal</a></p>
    </div>
######
landpageSendInviteView=
    <div class="centerdivnarrow">
     <p class="headline">Dokumentet <strong>$documenttitle$</strong> skickat!</p>
     <p>En inbjudan att underteckna har nu skickats 
        till $partyListButAuthor:noescape()$.
     </p>
     <p><a class="button" href="/">Skapa ett nytt avtal</a></p>
    </div>
######
landpageDocumentSavedView= 
    <div class="centerdivnarrow">
     <p>Ditt dokument är nu sparat. Du finner dokumentet under Arkiv när du loggar in.</p>
 
     <p>Vi hoppas att du är nöjd med vår tjänst hittills och att du är nyfiken på att själv använda 
        SkrivaPå för att skriva dina avtal. Därför erbjuder vi dig som ny kund möjligheten att testa 
        tjänsten genom tre fria avtal. Dina fria avtal förbrukas endast då ett avtal undertecknats av 
        alla parter.</p>

     <p>Börja redan nu! Ladda upp ditt avtal genom att klicka nedan.</p>
     <a class="bigbutton" href="/">Starta</a> 
    </div>
    
#####
landpageLoginForSaveView=    <div class="centerdivnarrow">
     <a class="headline">Login</a>
     <p>Ditt dokument är nu sparat. Du finner dokumentet under Avtal when you log in.</p>
     $loginBox()$
    </div>
#####
landpageRejectedView=
    <div class="centerdivnarrow">
      <p class="headline">Du har avvisat dokumentet <strong>$documenttitle$</strong>.</p>
      <p>Ett meddelande har skickats till $partyList:noescape()$.</p>
      <p><a href="/">Tillbaks till förstasidan</a></p>
    </div>
#####    
willCreateAccountForYouNoAccount= 
    <p>Dokumentet har sparats till ditt konto. 
      $loginBox()$
    </p>    
#####  
willCreateAccountForYouHasAccount= 
    <p>Du kan nu spara dokumentet på SkrivaPå, då är ditt dokument säkert lagrat och dessutom 
       kan du även i framtiden verifiera avtalet mot vår databas. Detta kostar ingenting. 
      <form action="$LinkLandpageSaved()$" method="get">
       <input class="button" type="submit" value="Skapa konto"/>
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