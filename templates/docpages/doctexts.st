###
contractpendingauthornotsignedinfoheader=
  $_contractpendingauthornotsignedinfoheader()$
###
offerpendingauthornotsignedinfoheader=
  $_offerpendingauthornotsignedinfoheader()$
###
orderpendingauthornotsignedinfoheader=
  $_orderpendingauthornotsignedinfoheader()$
###
contractpendingauthornotsignedinfotext=
  $_contractpendingauthornotsignedinfotext()$
###
offerpendingauthornotsignedinfotext=
  $_offerpendingauthornotsignedinfotext()$
###
orderpendingauthornotsignedinfotext=
  $_orderpendingauthornotsignedinfotext()$
###
contractpendingauthorinfoheader=$_contractpendingauthorinfoheader()$
###
offerpendingauthorinfoheader=$_offerpendingauthorinfoheader()$
###
orderpendingauthorinfoheader=$_orderpendingauthorinfoheader()$
###
contractpendingauthorinfotext=$if(iamauthor)$
                              $_contractpendingauthorinfotextIfiamauthor()$
                           $else$
                           $_contractpendingauthorinfotextIfElse()$
                           $endif$
###
offerpendingauthorinfotext=$_offerpendingauthorinfotext()$
###
orderpendingauthorinfotext=$_orderpendingauthorinfotext()$
###
contractcancelledinfoheader=$_contractcancelledinfoheader()$
###
offercancelledinfoheader=$_offercancelledinfoheader()$
###
ordercancelledinfoheader=$_ordercancelledinfoheader()$
###
contractcancelledinfotext=$authorfstname$ $authorsndname$ $_contractcancelledinfotext()$
###
offercancelledinfotext=$authorfstname$ $authorsndname$ $_offercancelledinfotext()$
###
ordercancelledinfotext=$authorfstname$ $authorsndname$ $_ordercancelledinfotext()$
###
contractsignedinfoheader=$_contractsignedinfoheader()$
###
offersignedinfoheader=$_offersignedinfoheader()$
###
ordersignedinfoheader=$_ordersignedinfoheader()$
###
contractsignedinfotext=$_contractsignedinfotext()$
###
offersignedinfotext=
    $signatories:{signatory | $if(!signatory.author)$ $signatory.fstname$ $signatory.sndname$ $endif$}$
    $_offersignedinfotextHasAccepted()$	 $documenttitle$ $_offersignedinfotextFrom()$  $authorfstname$ $authorsndname$ .
###
ordersignedinfotext=
    $signatories:{signatory | $if(!signatory.author)$ $signatory.fstname$ $signatory.sndname$ $endif$}$
    $_ordersignedinfotextHasConfirmed()$ $documenttitle$ $_ordersignedinfotextFrom()$ $authorfstname$
$authorsndname$ .
###
contractstatusinfotext=$_contractstatusinfotext()$
###
offerstatusinfotext=$_offerstatusinfotext()$
###
orderstatusinfotext=$_orderstatusinfotext()$
##########
documentInfoText=
                $if(pending)$
                     $if(notsignedbyme)$    
                        $if(iamauthor)$
                            <span class="header">
                                 $_documentInfoTextPendingNotsignedbymeImauthorHead()$
                            </span><br/>
                            <span class="text">
                              $process.statusinfotext$
                            </span>
                        $else$
                        <span class="header">
                              $process.pendingauthornotsignedinfoheader$
                        </span><br/>
                        <span class="text">
                            $process.pendingauthornotsignedinfotext$
                        </span>
                        $endif$

                     $elseif(signedbyme)$
                        <span class="header">
                             $_documentInfoTextPendingSignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">
                            $_documentInfoTextPendingSignedbymeImauthorBody()$
                        </span>
                     $else$
                         <span class="header">
                           $process.pendingauthorinfoheader$
                        </span><br/>
                        <span class="text">
                           $process.pendingauthorinfotext$
                        </span>
                     $endif$

                  $elseif(cancel)$

                     <span class="header">
                        $process.cancelledinfoheader$
                     </span><br/>
                     <span class="text">
                        $process.cancelledinfotext$
                     </span>
 
                  $elseif(datamismatch)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             $_documentInfoTextDatamismatchNotsignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">
                        
                                $_documentInfoTextDatamismatchNotsignedbymeImauthorBody()$

                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             $_documentInfoTextDatamismatchSignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">

                              $_documentInfoTextDatamismatchSignedbymeImauthorBody()$

                        </span>

                     $else$
                        <span class="header">
                             $_documentInfoTextDatamismatchElseImauthorHead()$
                        </span><br/>
                        <span class="text">

                              $_documentInfoTextDatamismatchElseImauthorBody()$

                        </span>
 
                     $endif$
 
                 $elseif(timedout)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             $_documentInfoTextTimedoutNotsignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextTimedoutNotsignedbymeImauthorBody()$
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             $_documentInfoTextTimedoutSignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextTimedoutSignedbymeImauthorBody()$
                        </span>

                     $else$
                        <span class="header">
                             $_documentInfoTextTimedoutElseImauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextTimedoutElseImauthorBody()$
                        </span>

                     $endif$

                  $elseif(rejected)$


                     $if(notsignedbyme)$    
                        <span class="header">
                             $_documentInfoTextRejectedNotsignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextRejectedNotsignedbymeImauthorBody()$
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             $_documentInfoTextRejectedSignedbymeImauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextRejectedSignedbymeImauthorBody()$
                        </span>

                     $else$
                        <span class="header">
                             $_documentInfoTextRejectedElseImauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextRejectedElseImauthorBody()$
                        </span>

                     $endif$

                  $elseif(signed)$
                        <span class="header">
                           $process.signedinfoheader$
                        </span><br/>
                        <span class="text">
                           $process.signedinfotext$
                        </span>
                 $elseif(awaitingauthor)$

                     $if(notsignedbyme)$    
                            <span class="header">
                                 $_documentInfoTextNotsignedbymeAwaitingauthorHead()$
                            </span><br/>
                            <span class="text">
                              $_documentInfoTextNotsignedbymeAwaitingauthorBody()$
                            </span>
                     $elseif(signedbyme)$
                        <span class="header">
                             $_documentInfoTextSignedbymeAwaitingauthorHead()$
                        </span><br/>
                        <span class="text">
                            $_documentInfoTextSignedbymeAwaitingauthorBody()$
                        </span>
                     $else$
                        <span class="header">
                             $_documentInfoTextElseAwaitingauthorHead()$
                        </span><br/>
                        <span class="text">
                           $_documentInfoTextElseAwaitingauthorBody()$
                        </span>
                     $endif$

                  $elseif(preparation)$
                        <span class="header">
                              $_documentInfoTextPreparationHead()$
                        </span><br/>
                        <span class="text">
                              $_documentInfoTextPreparationBody()$
                        </span>
                  $else$ 
                        <span class="header">
                             $_documentInfoTextElseHead()$
                        </span><br/>
                        <span class="text">
                        $_documentInfoTextElseBody()$<a href="mailto:support@skrivapa.se">$_documentInfoTextElseBodyMailSupport()$</a>
                        </span>
                  
                  $endif$
######                  
                
