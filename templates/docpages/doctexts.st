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
contractpendinginfotext=$_contractpendinginfotext()$
###
offerpendinginfotext=$_offerpendinginfotext()$
###
orderpendinginfotext=$_orderpendinginfotext()$
###
contractcancelledinfoheader=$_contractcancelledinfoheader()$
###
offercancelledinfoheader=$_offercancelledinfoheader()$
###
ordercancelledinfoheader=$_ordercancelledinfoheader()$
###
contractcancelledinfotext=$_contractcancelledinfotext()$
###
offercancelledinfotext=$_offercancelledinfotext()$
###
ordercancelledinfotext=$_ordercancelledinfotext()$
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
    $signatories:{signatory | $if(!signatory.author)$ $signatory.name$ $endif$}$
    $_offersignedinfotextHasAccepted()$	 $documenttitle$ $_offersignedinfotextFrom()$  $signatories:{signatory | $if(signatory.author)$ $signatory.name$ $endif$}$ .
###
ordersignedinfotext=
    $signatories:{signatory | $if(!signatory.author)$  $signatory.name$  $endif$}$
    $_ordersignedinfotextHasConfirmed()$ $documenttitle$ $_ordersignedinfotextFrom()$  $signatories:{signatory | $if(signatory.author)$ $signatory.name$ $endif$}$ .
###
contractstatusinfotext=$_contractstatusinfotext()$
###
offerstatusinfotext=$_offerstatusinfotext()$
###
orderstatusinfotext=$_orderstatusinfotext()$
##########
documentInfoText=
                $if(awaitingauthor)$

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

                $elseif(pending)$
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
                           $_documentpendinginfoheader()$
                        </span><br/>
                        <span class="text">
                           $process.pendinginfotext$
                           $if(isviewonly)$
                             $_documentpendingviewonly()$
                           $endif$
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
                        $_documentInfoTextElseBody()$<a href="mailto:support@scrive.com">$_documentInfoTextElseBodyMailSupport()$</a>
                        </span>

                  $endif$
######

