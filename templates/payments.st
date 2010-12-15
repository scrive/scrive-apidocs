####          PAYMENT ADMIN        
paymentsadminpage= <div style="text-align:center;margin-left:20%;margin-right:20%;"> 
                       <strong>     This page shows prices for all account types</strong> 
                        <div style="padding:20px;border:1px solid #DDDDDD;width:600px;margin-top:20px">
                          <table style="text-align:left"><tbody> 
                           <tr> 
                             <th style="width:300px;"/>
                             $models:{$it.modelname$}:strong():th()$
                           </tr>
                           <tr> 
                             <td style="padding-top:10px"><strong>Payment for account and subaccounts </strong> </td>
                           </tr>
                           <tr>
                             <td>For account: </td>
                              $models:{$it.foraccount$}:td()$ 
                           </tr>
                           <tr> 
                              <td>For subaccount: </td>  
                              $models:{$it.forsubaccount$}:td()$
                           </tr>
                           <tr> 
                              <td style="padding-top:20px"><strong>Payment for each signature </strong> </td>
                           </tr>
                           <tr> 
                              <td>For email signature:</td>   
                              $models:{$it.foremailsignature$}:td()$ 
                           </tr>
                           <tr>
                              <td>For eLeg signature:</td> 
                              $models:{$it.forelegsignature$}:td()$ 
                           </tr> 
                           <tr>  
                             <td>For For mobile signature: :</td>  
                              $models:{$it.formobiledignature$}:td()$
                           </tr> 
                           <tr> 
                              <td>For creadit card signature:</td>  
                              $models:{$it.forcrediteardsignature$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>For iPad signature: </td>  
                              $models:{$it.foripadsignature$}:td()$ 
                           </tr> 
                           <tr> 
                             <td style="padding-top:20px"><strong>Payment storage of signed documents</strong> </td>
                           </tr> 
                           <tr> 
                              <td>For Amazon: </td>     
                              $models:{$it.foramazon$}:td()$
                           </tr> 
                           <tr>  
                              <td>For TrustWeaver: </td>  
                              $models:{$it.fortrustweaver$}:td()$ </tr> 
                           <tr> 
                              <td style="padding-top:20px">  <strong>Payment storage of other documents</strong> </td></tr>   
                           <tr>  <td>Template:  </td>     
                               $models:{$it.fortemplate$}:td()$
                           </tr> 
                           <tr> 
                              <td> Draft:  </td>    
                                 $models:{$it.fordraft$}:td()$
                           </tr> 
                         <tbody></table>
                       </div></div>
###
paymentsadminpagesuperuser = <div style="text-align:center;margin-left:20%;margin-right:20%;"> 
                       <strong>   This page shows prices for all account types </strong>
                        <div style="padding:20px;border:1px solid #DDDDDD;width:600px;margin-top:20px">
                          <form method="POST" action="$changeaction$"> 
                          <table style="text-align:left"><tbody> 
                           <tr> 
                             <th style="width:300px;"/>
                             $models:{$it.modelname$}:strong():th()$
                           </tr>
                           <tr> 
                             <td style="padding-top:10px"><strong>Payment for account and subaccounts </strong> </td>
                           </tr>
                           <tr>
                             <td>For account: </td>
                             $models:{n | $input(iType = "text"; iName= join(fst="foraccount";snd=n.modelname); iValue=n.foraccount)$}:td()$
                           </tr>
                           <tr> 
                              <td>For subaccount: </td>  
                               $models:{n | $input(iType = "text"; iName= join(fst="forsubaccount";snd=n.modelname); iValue=n.forsubaccount)$}:td()$ 
                           </tr>
                           <tr> 
                              <td style="padding-top:20px"><strong>Payment for each signature </strong> </td>
                           </tr>
                           <tr> 
                              <td>For email signature:</td>   
                              $models:{n | $input(iType = "text"; iName= join(fst="foremailsignature";snd=n.modelname); iValue=n.foremailsignature)$}:td()$ 
                           </tr>
                           <tr>
                              <td>For eLeg signature:</td> 
                              $models:{n | $input(iType = "text"; iName= join(fst="forelegsignature";snd=n.modelname); iValue=n.forelegsignature)$}:td()$ 
                            </tr> 
                           <tr>  
                             <td>For For mobile signature: :</td>  
                              $models:{n | $input(iType = "text"; iName= join(fst="formobiledignature";snd=n.modelname); iValue=n.formobiledignature)$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>For creadit card signature:</td>  
                              $models:{n | $input(iType = "text"; iName= join(fst="forcrediteardsignature";snd=n.modelname); iValue=n.forcrediteardsignature)$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>For iPad signature: </td>  
                              $models:{n | $input(iType = "text"; iName= join(fst="foripadsignature";snd=n.modelname); iValue=n.foripadsignature)$}:td()$ 
                           </tr> 
                           <tr> 
                             <td style="padding-top:20px"><strong>Payment storage of signed documents</strong> </td>
                           </tr> 
                           <tr> 
                              <td>For Amazon: </td>     
                               $models:{n | $input(iType = "text"; iName= join(fst="foramazon";snd=n.modelname); iValue=n.foramazon)$}:td()$ 
                           </tr> 
                           <tr>  
                              <td>For TrustWeaver: </td>  
                               $models:{n | $input(iType = "text"; iName= join(fst="fortrustweaver";snd=n.modelname); iValue=n.fortrustweaver)$}:td()$ 
                           <tr> 
                              <td style="padding-top:20px">  <strong>Payment storage of other documents</strong> </td></tr>   
                           <tr>  <td>Template:  </td>     
                              $models:{n | $input(iType = "text"; iName= join(fst="fortemplate";snd=n.modelname); iValue=n.fortemplate)$}:td()$ 
                           </tr> 
                           <tr> 
                              <td> Draft:  </td>    
                              $models:{n | $input(iType = "text"; iName= join(fst="fordraft";snd=n.modelname); iValue=n.fordraft)$}:td()$ 
                          </tr> 
                         <tbody></table>
                         <BR/>
                         <BR/>
                         <input type="submit" style="width:200px;font-weight:bold" value="Change">
                         </form>
                       </div></div>

###