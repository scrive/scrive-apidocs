####
showUserMailAPI =
<form action="$linkmailapi$" method="post">
<div class="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li><a href="/account">$_HeadUserInfo()$</a></li>
       <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        $if(iscompanyadmin)$<li><a href="/account/company">$_CompanySettings()$</a></li>$endif$
       $if(iscompanyadmin)$<li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
       <li class="active"><a href="/account/mailapi">$_HeadMailApi()$</a></li>
       <li><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
   </ul>
   <div class="tab-container">
     <div class="tab-content account">
       <div class="col">
         <div class="account-header"><h2>$_MailAPISettings()$</h2></div>
         <div class="account-body">
           <div class="mailapi">
             <input type="checkbox" id="enableapicbox" name="api_enabled" $if(mailapienabled)$checked="checked"$endif$ />
             <label for="enableapicbox" style="margin-left: 5px">$_MailAPIEnable()$</label>
             $if(mailapienabled)$
               <div style="margin-top: 10px">
                 $_MailAPIActiveEmail()$
                 <ul id="mailapiemailslist">
                  <li><a href="mailto:contract$if(!production)$-staging$endif$+$mailapikey$@api.scrive.com">
                    contract$if(!production)$-staging$endif$+$mailapikey$@api.scrive.com
                   </a></li>
                   <li><a href="mailto:offer$if(!production)$-staging$endif$+$mailapikey$@api.scrive.com">
                     offer$if(!production)$-staging$endif$+$mailapikey$@api.scrive.com
                   </a></li>
                   <li><a href="mailto:order$if(!production)$-staging$endif$+$mailapikey$@api.scrive.com">
                     order$if(!production)$-staging$endif$+$mailapikey$@api.scrive.com
                   </a></li>
                 </ul>
                 <input type="checkbox" id="resetapikeycbox" name="reset_key" />
                 <label for="resetapikeycbox" style="margin-left: 5px">$_MailAPIResetKey()$</label>
               </div>
             $endif$
           </div>
         </div>
       </div>
       <div class="col">
         <div class="account-header"><h2>$_MailAPILimitHead()$</h2></div>
         <div class="account-body">
           <table class="mailapitable">
            <tr>
               <td>$_MailAPILimitsDaily()$</td>
               <td><input type="text" name="daily_limit" $if(mailapienabled)$ value="$mailapilimit$" $else$ value="N/A" disabled="disabled" $endif$ /></td>
            </tr>
            <tr>
              <td>$_MailAPILimitsProcessedToday()$</td>
              <td><input type="text" value="$if(mailapienabled)$$mailapisent$$else$N/A$endif$" disabled="disabled" /></td>
            </tr>
          </table>
          $if(mailapienabled)$
            <div class="mailapi">
              <input type="checkbox" id="resetsenttodaycbox" name="reset_senttoday" />
              <label for="resetsenttodaycbox" style="margin-left: 5px">$_MailAPILimitsResetToday()$</label>
            </div>
          $endif$
         </div>
       </div>
       $if(hascompanymailapi)$
       <div class="col">
         <div class="account-header"><h2>$_CompanyMailAPI()$</h2></div>
         <div class="account-body">
           <div class="mailapi">
               <div style="margin-top: 10px">
                 $_MailAPIActiveEmail()$
                 <ul id="mailapiemailslist">
                  <li><a href="mailto:contract$if(!production)$-staging$endif$+$company.mailapikey$@api.scrive.com">
                    contract$if(!production)$-staging$endif$+$company.mailapikey$@api.scrive.com
                   </a></li>
                   <li><a href="mailto:offer$if(!production)$-staging$endif$+$company.mailapikey$@api.scrive.com">
                     offer$if(!production)$-staging$endif$+$company.mailapikey$@api.scrive.com
                   </a></li>
                   <li><a href="mailto:order$if(!production)$-staging$endif$+$company.mailapikey$@api.scrive.com">
                     order$if(!production)$-staging$endif$+$company.mailapikey$@api.scrive.com
                   </a></li>
                 </ul>
                 <br />
                 $_MailAPILimitsDaily()$: $company.mailapilimit$ <br /><br />
                 $_MailAPILimitsProcessedToday()$ $company.mailapisent$ <br />
               </div>
           </div>
         </div>
       </div>
       $endif$
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
