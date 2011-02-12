##########
documentInfoText= $if(pending)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Granska
                        </span><br>
                        <span class="text">
                          Vänligen var noga med att granska dokumentet och kontrollera parterna till höger innan du undertecknar.                          
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Väntar på motpart
                        </span><br>
                        <span class="text">
                            Du har undertecknat och vi inväntar övriga parter.
                        </span>
                     $else$
                         <span class="header">
                             Ej färdigställt
                        </span><br>
                        <span class="text">
                           Du har skickat inbjudan att underteckna. Dokumentet har ännu inte undertecknats av samtliga parter.
                        </span>
                     $endif$

                  $elseif(cancel)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Dokument återkallat
                        </span><br>
                        <span class="text">
                           $authorname$  har återkallat sin inbjudan att underteckna dokumentet.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Dokument återkallat
                        </span><br>
                        <span class="text">
                           $authorname$ har återkallat sin inbjudan att underteckna dokumentet.
                        </span>

                     $else$
                        <span class="header">
                             Dokument återkallat
                        </span><br>
                        <span class="text">
                           $authorname$ har återkallat sin inbjudan att underteckna dokumentet.
                        </span>
 
                     $endif$
 
                 $elseif(timedout)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Förfallodatum har passerat
                        </span><br>
                        <span class="text">
                           Du kan inte längre underteckna dokumentet eftersom sista datum för att underteckna passerat.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Förfallodatum har passerat
                        </span><br>
                        <span class="text">
                           Samtliga parter undertecknade inte dokumentet i tid. Förfallodatum har passerat och därför har dokumentet avbrutits.
                        </span>

                     $else$
                        <span class="header">
                             Förfallodatum har passerat
                        </span><br>
                        <span class="text">
                           Samtliga parter undertecknade inte dokumentet i tid. Förfallodatum har passerat och därför har dokumentet avbrutits.
                        </span>

                     $endif$

                  $elseif(rejected)$


                     $if(notsignedbyme)$    
                        <span class="header">
                             Avvisat
                        </span><br>
                        <span class="text">
                           Dokumentet har avvisats av en motpart.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Avvisat
                        </span><br>
                        <span class="text">
                           Dokumentet har avvisats av en motpart.
                        </span>

                     $else$
                        <span class="header">
                             Avvisat
                        </span><br>
                        <span class="text">
                           Dokumentet har avvisats av en motpart
                        </span>

                     $endif$

                  $elseif(signed)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Färdigställt
                        </span><br>
                        <span class="text">
                           Samtliga parter har undertecknat dokumentet.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Färdigställt
                        </span><br>
                        <span class="text">
                           Samtliga parter har undertecknat dokumentet.
                        </span>

                     $else$
                        <span class="header">
                             Färdigställt
                        </span><br>
                        <span class="text">
                           Samtliga parter har undertecknat dokumentet.
                        </span>

                     $endif$

                  $elseif(awaitingauthor)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>
                     $else$
                        <span class="header">
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>
                     $endif$


                  $endif$
######                  
                