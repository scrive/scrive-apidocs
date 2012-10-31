###
  var paymentsBlocking = null;
  jQuery(function() {
    paymentsBlocking = Blocking();
    paymentsBlocking.show('.blocking-box');
  });
<div class='blocking-box'></div>
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
  mailheader=_paymentsDeclinedEmailHeader();
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
$_paymentsEmailBilledTo()$:
</p>
<p>
$fullname$<br />
$if(companyname)$$companyname$<br />$endif$
$email$
</p>
###
paymentsDeclinedEmail3=
<p>
$_paymentsDeclinedEmailCheck()$
</p>
##
paymentsExpiredEmail=
$_paymentsEmailExpiredSubject()$
$nicemail(
  mailheader=_paymentsEmailEmailHeader();
  mailp1=paymentsExpiredEmail1()
)$
###
paymentsExpiredEmail1=
$_paymentsEmailExpiredQuestions()$
###
