###
paymentsSignupEmail=
$_paymentsEmailThanksForChoosingScrive()$
$nicemail(
  mailheader=_paymentsEmailThanksForChoosingScrive();
  mailp1=paymentsSignupEmail1();
  mailspacing1="";
  mailp2=paymentsSignupEmail2();
  mailspacing2="";
  mailp3=paymentsSignupEmail3();
  mailspacing3="";
  mailp4=paymentsSignupEmail4()
)$
###
paymentsSignupEmail1=
<p>
$_paymentsEmailThanksForChoosingScrive()$. $_paymentsEmailSeeDetailsBelow()$
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
<strong>$fullname$</strong><br />
$if(companyname)$$companyname$<br />$endif$
<strong>$email$</strong>
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
  mailspacing1="";
  mailp2=paymentsDeclinedEmail2();
  mailspacing2 = "";
  mailp3=paymentsDeclinedEmail3();
  mailspacing3 = ""
)$
###
paymentsDeclinedEmail1=
<p>
$_paymentsDeclinedEmailSorry()$
</p>
###
paymentsDeclinedEmail2=
<p>
$_paymentsEmailBilledTo()$:
</p>
<p>
<strong>$fullname$</strong><br />
$if(companyname)$$companyname$<br />$endif$
<strong>$email$</strong>
</p>
###
paymentsDeclinedEmail3=
$_paymentsDeclinedEmailCheck()$
<br/>
<br/>
$nicebutton(link=concat(a1=ctxhostpart;a2="/account#subscription"); text=_paymentsDeclinedEmailButton())$
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
