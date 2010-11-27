######
mailCancelDocumentByAuthorTitle=Återkallelsenotis: $documenttitle$
####
mailCancelDocumentByAuthorStandardHeader=<p>Hej </p>
######
mailCancelDocumentByAuthorContent=    
        $header$
        <p>
                 $creatorname$ har valt att återkalla sin inbjudan att underteckna dokument $documenttitle$.
                 Därmed är avtalsprocessen avbruten och du kan inte längre underteckna dokumentet.    
        </p>
        $footer$
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
                av $partylist$. Avtalet är nu bindande.</p> 
             <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
             $poweredBySkrivaPaPara()$
        </span> 
#####
mailDocumentClosedForSignatoriesTitle=Bekräftelse: $documenttitle$    
#####
mailDocumentClosedForSignatoriesContent=     <span>
           <p>Hej <strong>$personname$</strong>,</p>
           <p>Dokumentet <strong>$documenttitle$</strong> har undertecknats 
             av $partylist$. Avtalet är nu bindande.</p> 
           <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
            $poweredBySkrivaPaPara()$
         </span>
####
mailRejectMailTitle=Avvisat: $documenttitle$
####
mailRejectMailContent= <span>
                      <p>Hej $authorname$,</p>
                      <p>$rejectorName$ har nekat att underteckna dokumentet <strong>$documenttitle$</strong>. 
                      Avtalsprocessen är därmed avbruten.</p>
                      $poweredBySkrivaPaPara()$
                    </span>       
###
mailInvitationToSignTitle=Inbjudan från $creatorname$ till $documenttitle$
###
mailInvitationToSignContent= 
                $header$
                <span>
                  <p><i>Översiktlig information</i><br/>
                   Parter: $partnersinfo$<br/>
                   Har undertecknat: <strong>$creatorname$</strong><br/>
                   $timetosigninfo$
                  </p>
                 <p><i>Så här går du vidare</i><br/>
                  1. Klicka på länken<br/>
                    <a href="$link$">$link$</a><br/>
                  2. Granska online<br/>
                  3. Underteckna<br/>
                  4. Färdig<br/> 
                 </p>
                </span>
                $footer$
######                
mailInvitationToSignDefaultHeader = <p>Hej, <BR/> $creatorname$ har bjudit in dig att underteckna 
                                       dokumentet Testavtal online via tjänsten SkrivaPå. </p>
####
remindMailSignedTitle=Hej $personname$      
###
remindMailSignedStandardHeader=     <span>
                                      <p>Hej <strong>$personname$</strong></p>
                                      <p><strong>$author$</strong> har begärt att vi ska skicka dokumentet <strong>$documenttitle$</strong> 
                                      till dig som du har undertecknat via tjänsten SkrivaPå.Dokumentet bifogas med detta mail.</p>
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
                             $header$
                             <span>
                              <p><i>Översiktlig information</i><br/>
                              Parter: $partnersinfo$<br/>
                              Har undertecknat: <strong>$creatorname$</strong><br/>
                              $timetosigninfo$
                              </p>
                             <p><i>Så här går du vidare</i><br/>
                             1. Klicka på länken<br/>
                                 <a href="$link$">$link$</a><br/>
                             2. Granska online<br/>
                             3. Underteckna<br/>
                             4. Färdig<br/>
                             </p>
                             </span>
                             $footer$                            
                             
####
newUserMailTitle=Nytt konto
####
newUserMailContent=
    <span>
      <p>Hej <strong>$personname$</strong>,</p>

      <p>Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har skapat ett konto hos oss. 
         Vi hoppas att du kommer att bli nöjd med våra tjänster. Tveka inte att höra av dig med 
         feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>

      <p>Användarnamn: <b>$email$</b><br/>
         Lösenord: <b>$password$</b><br/>
      </p>
      <p>
      <a href="$ctxhostpart$/login">$ctxhostpart$/login</a>
      </p>

      <p>Med vänliga hälsningar<br/>/Lukas Duczko och team <a href="$ctxhostpart$/">SkrivaPå</a>.
      </p>
     </span>
####
passwordChangeMailTitle=Nytt lösenord
####
passwordChangeMailContent= 
     <span>
      <p>Hej <strong>$personname$</strong>,</p>

      <p>Här kommer ditt nya lösenord. Vänligen ändra lösenordet så snart som möjligt.</p>

      <p>Användarnamn: <span style="color: orange; text-weight: bold">$email$</span><br/>
         Lösenord: <span style="color: orange; text-weight: bold">$password$</span><br/>
      </p>
      <p>
      <a href="$ctxhostpart$/login">$ctxhostpart$/login</a>
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
          Lösenord: <span style="color: orange; text-weight: bold">$password$</span><br/>
       </p>
 
       <p>
       <a href="$ctxhostpart$">$ctxhostpart$</a>
       </p>
       $poweredBySkrivaPaPara()$
       </span>
####
mailNewAccountCreatedByAdminTitle=Nytt konto
mailNewAccountCreatedByAdminContent=  
     <span>
      <p>Hej <strong>$personname$</strong>,</p>

      <p>$creatorname$ har bjudit in dig till ett konto på tjänsten SkrivaPå. </p>

      <p>SkrivaPå är ett webbaserat tredjepartsnotariat som gör det möjligt att underteckna, 
         hantera och arkivera avtal elektroniskt. SkrivaPå är som Gmail för dina avtal.</p>

      <p>Användarnamn: <b>$email$</b><br/>
         Lösenord: <b>$password$</b><br/>
      </p>
      <p>
       <a href="$ctxhostpart$">$ctxhostpart$</a>
      </p>
       $poweredBySkrivaPaPara()$
     </span>
####
####UTILS
timetosigninfo=  <span>Undertecknas senast: <strong>$time$</strong>.</span>
###
updateinglistwithauthor= <span>
                           <span class="partylistupdate"></span>
                           <span class="hidden authorname"> $creatorname$ </span>
                          </span> 
####