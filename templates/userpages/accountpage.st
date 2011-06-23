####
showUser =
<form action="$linkaccount$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li class="active"><a href="/account">$_HeadUserInfo()$</a></li>
       <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
       $if(issubaccounts)$<li><a href="/account/subaccount">$_Subaccounts()$</a></li>$endif$
       <li><a href="/account/sharing">$_sharing()$</a></li>
   </ul>
   <div class="tab-container">
       <div class="tab-content account">
           <div class="col">
               <div class="account-header">
                   <h2>$fstname$ $sndname$</h2>
                   <!-- <a href="javascript:;">Ladda upp ny bild...</a> -->
                   <div class="image">
                      $if(userimagelink)$
                       <img src="$userimagelink$" height="68" width="58" />
                      $endif$
                   </div>
               </div>
               <div class="account-body">
                   <table>
                       <tr>
                           <td>$_firstname()$:</td>
                           <td><input type="text" name="fstname" value="$fstname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>$_lastname()$:</td>
                           <td><input type="text" name="sndname" value="$sndname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>$_personnumber()$:</td>
                           <td><input type="text" name="personalnumber" value="$personalnumber$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>$_email()$:</td>
                           <td><input type="email" name="email" value="$email$" autocomplete="off" disabled=""/></td>
                       </tr>
                       <tr>
                           <td>$_phone()$:</td>
                           <td><input type="text" name="phone" value="$phone$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>$_companyposition()$:</td>
                           <td><input type="text" name="companyposition" value="$companyposition$" autocomplete="off" /></td>
                       </tr>
                   </table>
               </div>
           </div>
           <div class="col">
               <div class="account-header">
                   <h2>$companyname$</h2>
                   <!--<a href="javascript:;">Ladda upp ny logotyp...</a> -->
                   <div class="image">
                      $if(companyimagelink)$
                       <img src="$companyimagelink$" height="68" width="58" />
                      $endif$
                   </div>
               </div>
               <div class="account-body">
                   <table>
                       <tr>
                           <td>$_comapny()$:</td>
                           <td><input type="text" name="companyname" value="$companyname$" autocomplete="off" $if(hassupervisor)$disabled="disabled"$endif$/></td>
                       </tr>
                       <tr>
                           <td>$_orgnumber()$:</td>
                           <td><input type="text" name="companynumber" value="$companynumber$" autocomplete="off" $if(hassupervisor)$disabled="disabled"$endif$ /></td>
                       </tr>
                       <tr>
                           <td>$_address()$:</td>
                           <td><input type="text" name="address" value="$address$" autocomplete="off"  $if(hassupervisor)$disabled="disabled"$endif$/></td>
                       </tr>
                       <tr>
                           <td>$_zip()$:</td>
                           <td><input type="email" name="zip" value="$zip$" autocomplete="off"  $if(hassupervisor)$disabled="disabled"$endif$/></td>
                       </tr>
                       <tr>
                           <td>$_city()$:</td>
                           <td><input type="text" name="city" value="$city$" autocomplete="off"  $if(hassupervisor)$disabled="disabled"$endif$/></td>
                       </tr>
                       <tr>
                           <td>$_country()$:</td>
                           <td><input type="text" name="country" value="$country$" autocomplete="off"  $if(hassupervisor)$disabled="disabled"$endif$/></td>
                       </tr>
                   </table>
               </div>
           </div>
           <div class="account-footer">
                <a href="#" class="btn-small green submit">
                   <div class="left"></div>
                   <div class="label">
                       <p>$_SaveChanges()$</p>
                   </div>
                   <div class="right"></div>
                </a>
           </div>
           <div class="clearfix"></div>
           
       </div>
   </div>
</div>
</form>
####
