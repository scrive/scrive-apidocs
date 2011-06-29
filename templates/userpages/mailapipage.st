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
    ...
   </div>
</div>
</form>
####
