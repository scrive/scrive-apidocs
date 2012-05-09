####
showUserSecurity =
<form action="$linksecurity$" method="post">
<div class="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li><a href="/account">$_HeadUserInfo()$</a></li>
       <li class="active"><a href="/account/security">$_HeadAccountSettings()$</a></li>
        $if(iscompanyadmin)$<li><a href="/account/company">$_CompanySettings()$</a></li>$endif$
       $if(iscompanyadmin)$<li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
       <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
        <li><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
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
                           <td><input autocomplete="off" type="password" name="oldpassword" /></td>
                       </tr>
                       <tr>
                           <td>$_NewPassword()$</td>
                           <td><input autocomplete="off" type="password" name="password" /></td>
                       </tr>
                       <tr>
                           <td>$_RepeatNewPassword()$</td>
                           <td><input autocomplete="off" type="password" name="password2" /></td>
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
                   <td>$_UserRegion()$</td>
                   <td><select name="region">
                       <option $if(region.se)$ selected="" $endif$ value="REGION_SE">$_Sweden()$</option>
                       <option $if(region.gb)$ selected="" $endif$ value="REGION_GB">$_Britain()$</option>
                     </select>
                   </td>
                 </tr>
                 <!--
                 <tr>
                   <td>$_UserLanguage()$</td>
                   <td><select name="lang">
                       <option $if(lang.se)$ selected="" $endif$ value="LANG_SE">$_Swedish()$</option>
                       <option $if(lang.en)$ selected="" $endif$ value="LANG_EN">$_English()$</option>
                     </select>
                   </td>
                 </tr>
                 -->
               </table>
             </div>
           </div>
           <div class="col">
             <div class="account-header">
               <h2>$_userDocumentsSettings()$</h2>
             </div>
             <div class="account-body advancedview">
               <p>
                 <input type="checkbox" id="advancedMode"
                        name="advancedMode"
                        $if(advancedMode)$checked="checked"$endif$ />
                 <label for="advancedMode">$_dialogConfirmAdvancedCheck()$</label>
               </p>
              <p>
                 <input type="checkbox" id="footerCheckbox"
                        name="footerCheckbox"
                        $if(footer)$checked="checked"$endif$ />
                 <label for="footerCheckbox">$_userDocumentSettingsMailFooter()$</label>
                 <div class="customfooterbox" style="display:none">
                    <textarea id="customfooter" name="customfooter" style="width:350px;height:110px">$footer$</textarea>
                 </div>
                 <script>
                   jQuery(function() {
                   function toogleFooterBox() {
                    if (jQuery('#footerCheckbox').attr("checked"))
                            jQuery('.customfooterbox').show();
                        else
                            jQuery('.customfooterbox').hide();
                    }
                   jQuery("#footerCheckbox").click(function(){ toogleFooterBox()  });
                   jQuery("#customfooter").tinymce({
                                script_url: '/libs/tiny_mce/tiny_mce.js',
                                theme: "advanced",
                                theme_advanced_toolbar_location: "top",
                                theme_advanced_buttons1: "bold,italic,underline",
                                theme_advanced_buttons2: "",
                                convert_urls: false,
                                theme_advanced_toolbar_align: "left",
                                valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul"
                        });
                  toogleFooterBox();
                  });
                 </script>

               </p>
             </div>

           </div>
           <div class="clearfix"></div>

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


           <div class="clearfix"></div>

       </div>
   </div>
</div>
</form>
####
