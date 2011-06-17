####          PAYMENT ADMIN        
paymentsadminpage= <div style="text-align:center;margin-left:20%;margin-right:20%;"> 
                       <strong>$_paymentsadminpageHead()$</strong> 
                        <div style="padding:20px;border:1px solid #DDDDDD;width:600px;margin-top:20px">
                          <table style="text-align:left"><tbody> 
                           <tr> 
                             <th style="width:300px;"/>
                             $models:{$it.modelname$}:strong():th()$
                           </tr>
                           <tr> 
                             <td style="padding-top:10px"><strong>$_paymentsadminpageAccountSubHead()$</strong> </td>
                           </tr>
                           <tr>
                             <td>$_paymentsadminpageAccount()$</td>
                              $models:{$it.foraccount$}:td()$ 
                           </tr>
                           <tr> 
                              <td>$_paymentsadminpageSubaccount()$</td>  
                              $models:{$it.forsubaccount$}:td()$
                           </tr>
                           <tr> 
                              <td style="padding-top:20px"><strong>$_paymentsadminpageSignatureSubHead()$</strong> </td>
                           </tr>
                           <tr> 
                              <td>$_paymentsadminpageSignatureEmail()$</td>   
                              $models:{$it.foremailsignature$}:td()$ 
                           </tr>
                           <tr>
                              <td>$_paymentsadminpageSignatureEleg()$</td> 
                              $models:{$it.forelegsignature$}:td()$ 
                           </tr> 
                           <tr>  
                             <td>$_paymentsadminpageSignatureMobile()$</td>  
                              $models:{$it.formobiledignature$}:td()$
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpageSignatureCreditCard()$</td>  
                              $models:{$it.forcrediteardsignature$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpageSignatureIpad()$</td>  
                              $models:{$it.foripadsignature$}:td()$ 
                           </tr> 
                           <tr> 
                             <td style="padding-top:20px"><strong>$_paymentsadminpageStorageSubHead()$</strong> </td>
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpageStorageAmazon()$</td>     
                              $models:{$it.foramazon$}:td()$
                           </tr> 
                           <tr>  
                              <td>$_paymentsadminpageStorageTW()$</td>  
                              $models:{$it.fortrustweaver$}:td()$
                           </tr> 
                           <tr> 
                              <td style="padding-top:20px">  <strong>$_paymentsadminpageStorageOtherSubHead()$</strong> </td>
                           </tr>   
                           <tr>  <td>$_paymentsadminpageStorageOtherTemplate()$</td>     
                               $models:{$it.fortemplate$}:td()$
                           </tr> 
                           <tr> 
                              <td> $_paymentsadminpageStorageOtherDraft()$</td>    
                                 $models:{$it.fordraft$}:td()$
                           </tr> 
                         </tbody></table>
                       </div></div>
###
paymentsadminpagesuperuser = <div style="text-align:center;margin-left:20%;margin-right:20%;"> 
                       <strong>$_paymentsadminpagesuperuserHead()$</strong>
                        <div style="padding:20px;border:1px solid #DDDDDD;width:600px;margin-top:20px">
                          <form method="POST" action="$changeaction$"> 
                          <table style="text-align:left"><tbody> 
                           <tr> 
                             <th style="width:300px;"/>
                             $models:{$it.modelname$}:strong():th()$
                           </tr>
                           <tr> 
                             <td style="padding-top:10px"><strong>$_paymentsadminpagesuperuserAccountSubHead()$</strong> </td>
                           </tr>
                           <tr>
                             <td>$_paymentsadminpagesuperuserAccountAccount()$</td>
                             $models:{n | $input(iType = "text"; iName= join(fst="foraccount";snd=n.modelname); iValue=n.foraccount)$}:td()$
                           </tr>
                           <tr> 
                              <td>$_paymentsadminpagesuperuserAccountSubAccount()$</td>  
                               $models:{n | $input(iType = "text"; iName= join(fst="forsubaccount";snd=n.modelname); iValue=n.forsubaccount)$}:td()$ 
                           </tr>
                           <tr> 
                              <td style="padding-top:20px"><strong>$_paymentsadminpagesuperuserSignatureSubHead()$</strong> </td>
                           </tr>
                           <tr> 
                              <td>$_paymentsadminpagesuperuserSignatureEmail()$</td>   
                              $models:{n | $input(iType = "text"; iName= join(fst="foremailsignature";snd=n.modelname); iValue=n.foremailsignature)$}:td()$ 
                           </tr>
                           <tr>
                              <td>$_paymentsadminpagesuperuserSignatureEleg()$</td> 
                              $models:{n | $input(iType = "text"; iName= join(fst="forelegsignature";snd=n.modelname); iValue=n.forelegsignature)$}:td()$ 
                            </tr> 
                           <tr>  
                             <td>$_paymentsadminpagesuperuserSignatureMobile()$</td>  
                              $models:{n | $input(iType = "text"; iName= join(fst="formobiledignature";snd=n.modelname); iValue=n.formobiledignature)$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpagesuperuserSignatureCreditCard()$</td>  
                              $models:{n | $input(iType = "text"; iName= join(fst="forcrediteardsignature";snd=n.modelname); iValue=n.forcrediteardsignature)$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpagesuperuserSignatureIpad()$</td>  
                              $models:{n | $input(iType = "text"; iName= join(fst="foripadsignature";snd=n.modelname); iValue=n.foripadsignature)$}:td()$ 
                           </tr> 
                           <tr> 
                             <td style="padding-top:20px"><strong>$_paymentsadminpagesuperuserStorageSubHead()$</strong> </td>
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpagesuperuserStorageAmazon()$</td>     
                               $models:{n | $input(iType = "text"; iName= join(fst="foramazon";snd=n.modelname); iValue=n.foramazon)$}:td()$ 
                           </tr> 
                           <tr>  
                              <td>$_paymentsadminpagesuperuserStorageTW()$</td>  
                               $models:{n | $input(iType = "text"; iName= join(fst="fortrustweaver";snd=n.modelname); iValue=n.fortrustweaver)$}:td()$ 
                           <tr> 
                              <td style="padding-top:20px">  <strong>$_paymentsadminpagesuperuserStorageOtherSubHead()$</strong> </td></tr>   
                           <tr>  <td>$_paymentsadminpagesuperuserStorageOtherTemplate()$</td>     
                              $models:{n | $input(iType = "text"; iName= join(fst="fortemplate";snd=n.modelname); iValue=n.fortemplate)$}:td()$ 
                           </tr> 
                           <tr> 
                              <td>$_paymentsadminpagesuperuserStorageOtherDraft()$</td>    
                              $models:{n | $input(iType = "text"; iName= join(fst="fordraft";snd=n.modelname); iValue=n.fordraft)$}:td()$ 
                          </tr> 
                         </tbody></table>
                         <BR/>
                         <BR/>
                         <input type="submit" style="width:200px;font-weight:bold" value="Change">
                         </form>
                       </div></div>

###
