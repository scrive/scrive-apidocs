####
showUser =
<form action="$linkaccount$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li class="active"><a href="/account">Konto</a></li>
       <li><a href="/account/subaccount">Underkonton</a></li>
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
                   <h2>Personliga detaljer</h2>
                   <table>
                       <tr>
                           <td>Förnamn:</td>
                           <td><input type="text" name="fstname" value="$fstname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Efternamn:</td>
                           <td><input type="text" name="sndname" value="$sndname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Personnummer</td>
                           <td><input type="text" name="personalnumber" value="$personalnumber$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>E-post:</td>
                           <td><input type="email" name="email" value="$email$" autocomplete="off" disabled=""/></td>
                       </tr>
                       <tr>
                           <td>Telefonnummer:</td>
                           <td><input type="text" name="phone" value="$phone$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Befattning</td>
                           <td><input type="text" name="companyposition" value="$companyposition$" autocomplete="off" /></td>
                       </tr>
                   </table>
               </div>
               <div class="account-body">
                   <h2>Byt lösenord</h2>
                   <table>
                       <tr>
                           <td>Nuvarande lösen.:</td>
                           <td><input type="password" name="oldpassword" /></td>
                       <tr>
                           <td>Nytt lösenord:</td>
                           <td><input type="password" name="password" /></td>
                       </tr>
                       <tr>
                           <td>Upprepa nytt lösen.:</td>
                           <td><input type="password" name="password2" /></td>
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
                   <h2>Företagsdetaljer</h2>
                   <table>
                       <tr>
                           <td>Företag:</td>
                           <td><input type="text" name="companyname" value="$companyname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Org. nr:</td>
                           <td><input type="text" name="companynumber" value="$companynumber$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Adress:</td>
                           <td><input type="text" name="address" value="$address$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Postnummer:</td>
                           <td><input type="email" name="zip" value="$zip$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Postort:</td>
                           <td><input type="text" name="city" value="$city$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Land:</td>
                           <td><input type="text" name="country" value="$country$" autocomplete="off" /></td>
                       </tr>
                   </table>
               </div>
           </div>
           <div class="clearfix"></div>
           <div class="account-footer">
               <a href="#" class="btn-big green submit">
                   <div class="left"></div>
                   <div class="label">
                       <p>Spara</p>
                   </div>
                   <div class="right"></div>
               </a>
           </div>
       </div>
   </div>
</div>
####
