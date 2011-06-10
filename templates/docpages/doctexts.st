###
contractpendingauthornotsignedinfoheader=
  Granska
###
offerpendingauthornotsignedinfoheader=
  Granska innan accept
###
orderpendingauthornotsignedinfoheader=
  Granska innan bekräftelse
###
contractpendingauthornotsignedinfotext=
  Vänligen var noga med att granska dokumentet och kontrollera parterna till höger innan du undertecknar.
###
offerpendingauthornotsignedinfotext=
  Vänligen var noga med att granska offerten innan du accepterar.
###
orderpendingauthornotsignedinfotext=
  Vänligen var noga med att granska ordern innan du bekräftar.
###
contractpendingauthorinfoheader=Väntar
###
offerpendingauthorinfoheader=Väntar på accept
###
orderpendingauthorinfoheader=Väntar på bekräftelse
###
contractpendingauthorinfotext=Du har skickat inbjudan att underteckna. Dokumentet har ännu inte undertecknats av samtliga parter.
###
offerpendingauthorinfotext=Du har skickat offerten. Offerten har ännu inte accepterats av mottagaren.
###
orderpendingauthorinfotext=Du har skickat orderbekräftelsen. Orderbekräftelsen har ännu inte bekräftats av din mottagare.
###
contractcancelledinfoheader=Dokument återkallat
###
offercancelledinfoheader=Offert återkallad
###
ordercancelledinfoheader=Order återkallad
###
contractcancelledinfotext=$authorname$  har återkallat sin inbjudan att underteckna dokumentet.
###
offercancelledinfotext=$authorname$ har återkallat offerten. Den går således inte att acceptera.
###
ordercancelledinfotext=$authorname$ har återkallat ordern. Den går således in att acceptera.
###
contractsignedinfoheader=Färdigställt
###
offersignedinfoheader=Offert accepterad
###
ordersignedinfoheader=Ordern bekräftad
###
contractsignedinfotext=Samtliga parter har undertecknat dokumentet.
###
offersignedinfotext=
    $signatories:{signatory | $if(!signatory.author)$ $signatory.fstname$ $signatory.sndname$ $endif$}$
    har accepterat offerten $documenttitle$  från $authorfstname$ $authorsndname$ .
###
ordersignedinfotext=
    $signatories:{signatory | $if(!signatory.author)$ $signatory.fstname$ $signatory.sndname$ $endif$}$
    har bekräftat ordern $documenttitle$  från $authorfstname$ $authorsndname$ .
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
                              $process.pendingauthornotsignedinfoheader$
                        </span><br>
                        <span class="text">
                            $process.pendingauthornotsignedinfotext$
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
                           $process.pendingauthorinfoheader$
                        </span><br>
                        <span class="text">
                           $process.pendingauthorinfotext$
                        </span>
                     $endif$

                  $elseif(cancel)$

                     <span class="header">
                        $process.cancelledinfoheader$
                     </span><br>
                     <span class="text">
                        $process.cancelledinfotext$
                     </span>
 
                  $elseif(datamismatch)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Avtalsprocessen avbruten
                        </span><br>
                        <span class="text">
                        
                                Avtalsprocessen är avbruten på grund
                                av felaktig matchning mellan en
                                undertecknande part och dennes
                                e-legitimation.

                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Avtalsprocessen avbruten
                        </span><br>
                        <span class="text">

                              Avtalsprocessen är avbruten på grund av
                              felaktig matchning mellan en
                              undertecknande part och dennes
                              e-legitimation.

                        </span>

                     $else$
                        <span class="header">
                             Avtalsprocessen avbruten
                        </span><br>
                        <span class="text">

                              Avtalsprocessen är avbruten på grund av
                              felaktig matchning mellan en
                              undertecknande part och dennes
                              e-legitimation.

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
                        <span class="header">
                           $process.signedinfoheader$
                        </span><br>
                        <span class="text">
                           $process.signedinfotext$
                        </span>
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

                  $elseif(preparation)$
                        <span class="header">
                              Förberedelse
                        </span><br>
                        <span class="text">
                              Dokumentet är i förberedelsefasen.
                        </span>
                  $else$ 
                        <span class="header">
                             Problem
                        </span><br>
                        <span class="text">
                        Problem Ledsen, det har uppstått ett problem att rendera sigillet på PDF:en. Vänliga ladda upp dokumentet igen. Om problemet upprepar sig var vänlig kontakta <a href="mailto:support@skrivapa.se">support@skrivapa.se</a>
                        </span>
                  
                  $endif$
######                  
                
