####
showUserMailAPI =
<form action="$linkmailapi$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li><a href="/account">$_HeadUserInfo()$</a></li>
       <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
       $if(issubaccounts)$<li><a href="/account/subaccount">$_Subaccounts()$</a></li>$endif$
       <li><a href="/account/sharing">$_sharing()$</a></li>
       <li class="active"><a href="/account/mailapi">$_HeadMailApi()$</a></li>
   </ul>
   <div class="tab-container">
     <div class="tab-content account">
       <div class="col">
         <div class="account-header"><h2>Settings</h2></div>
         <div class="account-body">
           <div class="mailapi">
             <input type="checkbox" id="enableapicbox" name="api_enabled" $if(mailapienabled)$checked="checked"$endif$ />
             <label for="enableapicbox" style="margin-left: 5px">Enable Mail API</label>
             $if(mailapienabled)$
               <div style="margin-top: 10px">
                 Active e-mail addresses:
                 <ul id="mailapiemailslist">
                  <li><a href="mailto:contract+$mailapikey$@api.skrivapa.se">
                    contract+$mailapikey$@api.skrivapa.se
                   </a></li>
                   <li><a href="mailto:offer+$mailapikey$@api.skrivapa.se">
                     offer+$mailapikey$@api.skrivapa.se
                   </a></li>
                   <li><a href="mailto:order+$mailapikey$@api.skrivapa.se">
                     order+$mailapikey$@api.skrivapa.se
                   </a></li>
                 </ul>
                 <input type="checkbox" id="resetapikeycbox" name="reset_key">
                 <label for="resetapikeycbox" style="margin-left: 5px">Reset API key</label>
               </div>
             $endif$
           </div>
         </div>
       </div>
       <div class="col">
         <div class="account-header"><h2>Limits</h2></div>
         <div class="account-body">
           <table class="mailapitable">
            <tr>
               <td>Daily limit of processed e-mails</td>
               <td><input type="text" name="daily_limit" $if(mailapienabled)$ value="$mapidailylimit$" $else$ value="N/A" disabled="disabled" $endif$ />
             </tr>
            <tr>
              <td>E-mails processed today:</td>
              <td><input type="text" value="$if(mailapienabled)$$mapisenttoday$$else$N/A$endif$" disabled="disabled" /></td>
            </tr>
          </table>
          $if(mailapienabled)$
            <div class="mailapi">
              <input type="checkbox" id="resetsenttodaycbox" name="reset_senttoday">
              <label for="resetsenttodaycbox" style="margin-left: 5px">Reset counter of e-mails processed today</label>
            </div>
          $endif$
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
