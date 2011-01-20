######
mailCancelDocumentByAuthorTitle=Återkallelsenotis: $documenttitle$
####
mailCancelDocumentByAuthorStandardHeader=<p>Hej </p>
######
mailCancelDocumentByAuthorContent=    
        $header:noescape()$
        <p>
                 $creatorname$ har valt att återkalla sin inbjudan att underteckna dokument $documenttitle$.
                 Därmed är avtalsprocessen avbruten och du kan inte längre underteckna dokumentet.    
        </p>
        $footer:noescape()$
#####
mailDocumentAwaitingForAuthorTitle=Bekräftelse: $documenttitle$ 
######
mailDocumentAwaitingForAuthorContent=  <span>
             <p>Hej <strong>$authorname$</strong>,</p>
             <p>Dokumentet <strong>$documenttitle$</strong> has been signed by all signatories. 
                There were fields for the signatories to fill out. The contract is being held for your final review.  
                Please follow the link below to read and confirm the information for each signatory.</p>
             <a href="$documentlink$">$documenttitle$</a>
             $poweredBySkrivaPaPara()$
        </span> 

#####
mailDocumentClosedForAuthorTitle=Bekräftelse: $documenttitle$ 
#####
mailDocumentClosedForAuthorContent=    <span>
          <p>Hej <strong>$authorname$</strong>,</p>
             <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats 
                av $partylist:noescape()$. Avtalet är nu bindande.</p> 
             <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
             $poweredBySkrivaPaPara()$
        </span> 
#####
mailDocumentClosedForSignatoriesTitle=Bekräftelse: $documenttitle$    
#####
mailDocumentClosedForSignatoriesContent=     <span>
           <p>Hej <strong>$personname$</strong>,</p>
           <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats 
             av $partylist:noescape()$. Avtalet är nu bindande.</p> 
           <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
            $poweredBySkrivaPaPara()$
         </span>
####
mailRejectMailTitle=Avvisat: $documenttitle$
####
mailRejectMailContent= <span>
                      <p>Hej $authorname$,</p>
                      <p>$rejectorName$ har avvisat att underteckna dokumentet <strong>$documenttitle$</strong>. 
                      Avtalsprocessen är därmed avbruten.</p>
                      $poweredBySkrivaPaPara()$
                    </span>       
###
mailInvitationToSignTitle=Inbjudan från $creatorname$ till $documenttitle$
###
mailInvitationToSignContent= 
                $header:noescape()$
                <span>
                  <p><i>Översiktlig information</i><br/>
                   Parter: $partnersinfo:noescape()$<br/>
                   $whohadsignedinfo:noescape()$<br/>
                   $timetosigninfo:noescape()$
                  </p>
                 <p><i>Så här går du vidare</i><br/>
                  1. Klicka på länken <br/>
                    <a href="$link$">$link$</a> <br/>
                    <br/>
                  2. Granska online<br/>
                  3. Underteckna<br/>
                  4. Färdig</p>
		 <p>
		 <i>Detta mail får inte vidarebefordras.</i>
		 </p>
                </span>
                $footer:noescape()$
###        
mailInvitationToSignDefaultHeader = <p>Hej $personname$, <br/><br/> $creatorname$ har bjudit in dig att underteckna 
                                       dokumentet Testavtal online via tjänsten SkrivaPå. </p>
####
remindMailSignedTitle=Hej $personname$      
###
remindMailSignedStandardHeader=     <span>
                                      <p>Hej <strong>$personname$</strong></p>
                                      <p><strong>$author$</strong> har begärt att vi ska skicka dokumentet <strong>$documenttitle$</strong> 
                                      till dig som du har undertecknat via tjänsten SkrivaPå. Dokumentet bifogas med detta mail.</p>
                                    </span>
####
remindMailNotSignedTitle=Hej $personname$
########              
remindMailNotSignedStandardHeader=   <span>
  <p>Hej <strong>$personname$</strong></p>
  <p><strong>$author$</strong> vill påminna dig om att du inte undertecknat dokument <strong>$documenttitle$</strong> ännu.   </p>  
  </span>

#####
remindMailNotSignedContent=
                             $header:noescape()$
                             <span>
                              <p><i>Översiktlig information</i><br/>
                              Parter: $partnersinfo:noescape()$<br/>
                              $whohadsignedinfo:noescape()$<br/>
                              $timetosigninfo:noescape()$
                              </p>
                             <p><i>Så här går du vidare</i><br/>
                             1. Klicka på länken<br/>
                                 <a href="$link$">$link$</a><br/>
                             2. Granska online<br/>
                             3. Underteckna<br/>
                             4. Färdig</p>
			    <p>
			    <i>Detta mail får inte vidarebefordras.</i>
			    </p>
                             </span>
                             $footer:noescape()$                            
                             
####
newUserMailTitle=Nytt konto
####
newUserMailContent=
    <span>
      <p>Hej <strong>$personname$</strong>,</p>

      <p>Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har skapat ett konto hos oss. 
         Vi hoppas att du kommer att bli nöjd med våra tjänster.
         Tveka inte att höra av dig med feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>
      <p>Användarnamn: <a style="color:#0000CC;font-weight:bold">$email$</a><br/>
      </p>
      <p>Du aktiverar ditt konto genom att klicka på länken nedan. Länken är giltig i 12 timmar. <BR/>
         <a href="$ctxhostpart$$activatelink$">$ctxhostpart$$activatelink$</a>
      </p>
      <p>Med vänliga hälsningar<br/>/Lukas Duczko och team <a href="$ctxhostpart$/">SkrivaPå</a>.
      </p>
     </span>
####
passwordChangeMailTitle=Nytt lösenord
####
passwordChangeMailContent= 
     <span>
      <p>Hej $personname$,</p>

      <p>Du har begärt ett nytt lösenord. Vänligen klicka på länken nedan för att välja ett nytt lösenord.<br/>  
	<a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a>
      </p>
       $poweredBySkrivaPaPara()$
     </span>
####
inviteSubaccountMailTitle=Inbjudan från $supervisorname$ till underkonto
####
inviteSubaccountMailContent=
       <span>
       <p>Hej <strong>$personname$</strong>,</p>
 
       <p><strong>$supervisorname$</strong> har bjudit in dig att öppna ett konto på SkrivaPå genom vilket du kan
          skriva avtal för <strong>$companyname$</strong>. Observera att detta konto inte är
          avsett för privat bruk.</p>     
       <p>Användarnamn: <span style="color: orange; text-weight: bold">$email$</span><br/> 
       </p>
       <p>Vänligen klicka på länken för att aktivera kontot:<br/>  
	<a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a>
      </p>
       <p>
       <a href="$ctxhostpart$">$ctxhostpart$</a>
       </p>
       $poweredBySkrivaPaPara()$
       </span>
####
mailNewAccountCreatedByAdminTitle=Nytt konto
###
mailNewAccountCreatedByAdminContent=  
     <span>
      <p>Hej <strong>$personname$</strong>,</p>

      <p>$creatorname$ har bjudit in dig till ett konto på tjänsten SkrivaPå. </p>

      <p>SkrivaPå är ett webbaserat tredjepartsnotariat som gör det möjligt att underteckna, 
         hantera och arkivera avtal elektroniskt. SkrivaPå är som Gmail för dina avtal.</p>

	<p>Användarnamn: <span style="color: orange; text-weight: bold">$email$</span><br/> 
       </p>

      <p>Vänligen klicka på länken för att aktivera kontot:<br/>  
	<a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a>
      </p>
      <p>
       <a href="$ctxhostpart$">$ctxhostpart$</a>
      </p>
       $poweredBySkrivaPaPara()$
     </span>
####
invitationMailUndeliveredTitle =Felaktig e-post
###
invitationMailUndeliveredContent = 
	<span>
	<p>Hej $authorname$,</p>
	<p>Adressen $email$ existerar inte och därför kunde vi inte leverera din inbjudan att underteckna via e-post. 		
	Processen har därmed avbrutits. Gör så här om du vill börja om:</p>

	<p>1. Kontrollera vilken som är mottagarens korrekta adress<br/>  
	   2. Klicka på dokumentet i ditt arkiv eller på följande länk<br/>  
	  <a href="$ctxhostpart$$unsigneddoclink$">$ctxhostpart$$unsigneddoclink$</a> </p>

	<p>3. Klicka "Starta om"<br/>
	   4. Skriv in rätt e-postadress och skicka igen</p>
       
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
   <p>We are sorry for the inconvenience
   $poweredBySkrivaPaPara()$

###
passwordChangeLinkMailTitle=Nytt lösenord
###
passwordChangeLinkMailContent=   <span>
      <p>Hej $personname$,</p>
      <p>Du har begärt att ändra ditt lösenord på SkrivaPå. Vänligen klicka på följande länk för att gå vidare:</p>
      <a href="$ctxhostpart$$passwordlink$">$ctxhostpart$$passwordlink$</a>
       $poweredBySkrivaPaPara()$
     </span>
###
###
####UTILS
timetosigninfo=  <span>Undertecknas senast: <strong>$time$</strong>.</span>
###
updateinglistwithauthor= <span>
                           <span class="partylistupdate"></span>
                           <span class="hidden authorname"> $creatorname$ </span>
                          </span> 
####
noonehadsignedtext=Ingen har undertecknat dokumentet ännu
####
whohadsignedinfoformail =  $if(signedlist)$
                            Har undertecknat: $signedlist:noescape()$
                           $else$
                            $noonehadsignedtext()$
                           $endif$ 
####
####
whohadsignedinfoforpreview = <span class="authornamewhennotsecretary" alt="$noonehadsignedtext()$" okprefix="Har undertecknat:"></span>
####
