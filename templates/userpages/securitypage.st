####
showUserSecurity =
<form action="$linksecurity$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li><a href="/account">$_HeadUserInfo()$</a></li>
       <li class="active"><a href="/account/security">$_HeadAccountSettings()$</a></li>
       $if(issubaccounts)$<li><a href="/account/subaccount">$_Subaccounts()$</a></li>$endif$
       <li><a href="/account/sharing">$_sharing()$</a></li>
       <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
   </ul>
   <div class="tab-container">
       <div class="tab-content account">
           <div class="col">
               <div class="account-header">
                   <h2>$_password()$</h2>
               </div>
               <div class="account-body">
                   <table>
                       <tr>
                           <td>$_PresentPassword()$</td>
                           <td><input type="password" name="oldpassword" /></td>
                       </tr>
                       <tr>
                           <td>$_NewPassword()$</td>
                           <td><input type="password" name="password" /></td>
                       </tr>
                       <tr>
                           <td>$_RepeatNewPassword()$</td>
                           <td><input type="password" name="password2" /></td>
                       </tr>
                   </table>
               </div>
           </div>

           <div class="col">
               <div class="account-header">
                  <h2>$_LanguageSettings()$</h2>
               </div>
               <div class="account-body">
                   <table>
                       <tr>
                           <td>$_UserLanguage()$</td>
                           <td><select name="lang">
                                     <option $if(lang.se)$ selected="" $endif$ value="LANG_SE">$_Swedish()$</option>
                                     <option $if(lang.en)$ selected="" $endif$ value="LANG_EN">$_English()$</option>
                               </select>
                            </td>
                       </tr>
                   </table>
               </div>
             <div class="account-footer">
                <input type="submit" class="hidden"/>
                <a href="#" class="btn-small green submit">
                   <div class="left"></div>
                   <div class="label">
                       <p>$_SaveSettings()$</p>
                   </div>
                   <div class="right"></div>
                </a>
             </div>

           </div>

           <div class="clearfix"></div>
       </div>
   </div>
</div>
</form>
####
