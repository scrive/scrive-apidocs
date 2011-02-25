####          PAYMENT PROCESSED BY PAYEX        
paymentsView=<table><tbody>
                <tr><th style="width:150px"/><th style="width:400px"/><th style="width:200px"/><tr>
                $payments:{ payment | 
                    <tr>
                    <td>
                       <a href="$payment.pvLink$"> 
                       $payment.pvId$
                       </a>
                    </td>
                    <td>
                       $payment.pvPositions:br()$
                    </td>
                    <td>
                       $if(payment.pvIsSend)$
                       Send
                       $elseif(payment.pvIsWaiting)$
                       Waiting
                       $elseif(payment.pvIsFinished)$
                       Finished
                       $elseif(payment.pvIsFailed)$
                       Error   
                       $elseif(payment.pvIsDropped)$
                       Dropped
                       $elseif(payment.pvIsInvoiceable)$
                       Waiting for invoice generation
                       $else$
                       Error (New payment not processed)
                       $endif$
                    </td>
                    </tr>}$
                 </tbody>
             </table>
                 <form>
                       <input type="submit" name="invoice" value="Generate invoice">
                 </form> 
####
paymentView=   <div>
               <div style="text-align:center;width:100%"><a href="$payment.pvLink$"> 
                  <strong>  $payment.pvId$ </strong>
               </a>
               <span style="float:right;">
                       $if(payment.pvIsSend)$
                       Send
                       $elseif(payment.pvIsWaiting)$
                       Waiting
                       $elseif(payment.pvIsFinished)$
                       Finished
                       $elseif(payment.pvIsDropped)$
                       Dropped
                       $elseif(payment.pvIsInvoiceable)$
                       Waiting for invoice generation
                       $elseif(payment.pvIsFailed)$
                       Error   
                       $else$
                       Error (New payment not processed)
                       $endif$
              </span>      
              </div>
              <BR/>
              <BR/>
              
              <span style="float:right;text-decoration:underline">
              $payment.pvPositions:br()$
              </span>     
               $if(payment.pvIsSend)$
                  <a href="$payment.pvPayExUrl$" > <input type="button" value="Pay with PayEx"/></a>
               $endif$
              $if(payment.pvIsFailed)$
                 <form action="$payment.pvLink$">
                       <input type="submit" name="unfail" value="Reactivate">
                 </form> 
              $endif$
              $if(payment.pvIsWaiting)$
                 <form action="$payment.pvLink$">
                       <input type="submit" name="initial" value="Send">
                       Create also an agreement for future payment: <input type="checkbox" name="agreement">
                 </form> 
              $endif$
                 <form action="$payment.pvLink$">
                       <input type="submit" name="cancel" value="Back to waiting">
                 </form> 
              
             </div>
####
paymentForSigningPosition= Payment for all signatories on document $documenttitle$
####
mailNewPaymentTitle=New payment
####
mailNewPaymentContent=We have created new payment.<BR/>
                        <BR/>
                        Payment can be checked by clicking this following link: <br/>
                        <a href="$ctxhostpart$$payment.pvLink$"> $ctxhostpart$$payment.pvLink$ </a> <br/>
                        or by choosing payment info tab in SkrivaPa. <br/>
                        <br/>
                        If You are tired of having to pay every time after finalizing a document <br/>
                        please conside one of our payment plans <br/>
                        <br/>
                       $poweredBySkrivaPaPara()$