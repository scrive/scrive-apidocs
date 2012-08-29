##
subscriptionDashboard =
<script>
  jQuery(function(){bootPaymentsDashboard('.js-paymentsdashboard')});
</script>
<div class="tab-viewer paymentsdashboard">
  <div class="tab-header">
  </div>
  <ul class="tabs">
    <li class="float-left"><a href="/account">$_HeadUserInfo()$</a></li>
    <li class="float-left"><a href="/account/security">$_HeadAccountSettings()$</a></li>
    $if(iscompanyadmin)$<li class="float-left"><a href="/account/company">$_CompanySettings()$</a></li>$endif$
    $if(iscompanyadmin)$<li class="float-left"><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
    <li class="float-left"><a href="/account/mailapi">$_HeadMailApi()$</a></li>
    <li class="float-left"><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
    <!--<li class="float-left"><a href="/oauth/dashboard">API</a></li>-->
    <li class="active float-left"><a href="/payments/dashboard">$_Subscription()$</a></li>
    <li class="float-right"/>
  </ul>
  <div class="tab-container">
    <div class="tab-content account js-paymentsdashboard payments-dashboard">
    </div>
  </div>
</div>
##
paymentsSignupEmail=
$_paymentsEmailThanksForChoosingScrive()$
$nicemail(
  mailheader=_paymentsEmailThanksForChoosingScrive();
  mailp1=paymentsSignupEmail1();
  mailp2=paymentsSignupEmail2();
  mailp3=paymentsSignupEmail3();
  mailp4=paymentsSignupEmail4()
)$
###
paymentsSignupEmail1=
<p>
$_paymentsEmailThanksForChoosingScrive()$. $_paymentsEmailSeeDetailsBelow()$:
</p>
<p>
$_paymentsEmailDescription()$: $planname$<br />
$if(quantity)$$_CompanyAccounts()$: $quantity$<br />$endif$
$_paymentsEmailTotal()$: $total$ $currency$ $_paymentPlanPerMonth()$<br />
</p>
###
paymentsSignupEmail2=
<p>
$_paymentsEmailCreditCardCharged()$: $total$ $currency$ <br />
$_paymentsEmailTransactionDate()$: $startdate$<br /><br />
$_paymentsEmailNextTransactionDate()$: $enddate$
</p>
###
paymentsSignupEmail3=
<p>
$_paymentsEmailBilledTo()$:
</p>
<p>
$fullname$<br />
$if(companyname)$$companyname$<br />$endif$
$email$
</p>
###
paymentsSignupEmail4=
<p>
$_paymentsEmailQuestion()$
</p>
###
paymentsDeclinedEmail=
$_paymentsDeclinedEmailSubject()$
$nicemail(
  mailheader=_paymentsDeclinedEmailHeader()$
  mailp1=paymentsDeclinedEmail1();
  mailp2=paymentsDeclinedEmail2();
  mailp3=paymentsDeclinedEmail3()
)$
###
paymentsDeclinedEmail1=
<p>
$_paymentsDeclinedEmailSorry()$
</p>
###
<p>
$_paymenstEmailTransactionDate()$: $date$<br />
$_paymentsEmailTotal()$: $total$ $currency$
</p>
###
paymentsDeclinedEmail2=
<p>
$_paymentsEmailBilledTo:
</p>
<p>
$fullname$<br />
$if(companyname$$companyname$<br />$endif$
$email$
</p>
###
paymentsDeclinedEmail3=
<p>
$_paymentsDeclinedEmailCheck()$
</p>
##
paymentsExpiredEmail=
$_paymentsEmailThanksForChoosingScrive()$
$nicemail(
  mailheader=_paymentsEmailThanksForChoosingScrive();
  mailp1=paymentsSignupEmail1();
  mailp2=paymentsSignupEmail2();
  mailp3=paymentsSignupEmail3();
  mailp4=paymentsSignupEmail4()
)$
###