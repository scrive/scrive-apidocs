##########
documentInfoText=
                $if(pending)$
                     $if(notsignedbyme)$    
                        $if(iamauthor)$
                            <span class="header">
                                 Väntar
                            </span><br>
                            <span class="text">
                                 Du har valt att underteckna sist. Vänligen invänta övriga parters underskrift.
                            </span>
                        $else$
                        <span class="header">
                             Granska
                        </span><br>
                        <span class="text">
                          Vänligen var noga med att granska dokumentet och kontrollera parterna till höger innan du undertecknar.
                        </span>
                        $endif$

                     $elseif(signedbyme)$
                        <span class="header">
                             Väntar på motpart
                        </span><br>
                        <span class="text">
                            Du har undertecknat och vi inväntar övriga parter.
                        </span>
                     $else$
                         <span class="header">
                             Väntar
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
                                 Granska
                            </span><br>
                            <span class="text">
                              Vänligen kontrollera dokumentet och parternas uppgifter och underteckna längst ned.
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
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>
                     $endif$

                  $else$ 
                        <span class="header">
                             Problem
                        </span><br>
                        <span class="text">
                              Ledsen, det har uppstått ett problem med att rendera sigillet på PDF:en.
                              Våra tekniker tittar på detta just nu. Om problemet upprepar sig var
                              vänlig kontakta <a>support@skrivapa.se</a>
                        </span>
                  
                  $endif$
######                  
                