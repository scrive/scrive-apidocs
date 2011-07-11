###
termsPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_termsPageTitle()$</title>
        <meta name="description" content="$_termsPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      <body>
         $standardBodyWithQueryCol(unwrappedbody=termsPageContents())$
      </body>
    </html>
###
termsPageContents=

					<div class="nicetext">
<h1>$_termsPageH1()$</h1>
<br />
<p>$_termsPageH1Body()$</p>
<br />
<h2>$_termsPageH2Info()$</h2>
<br />
<p>$_termsPageH2InfoBody()$</p>
<br />
<h2>$_termsPageH2Useraccounts()$</h2>
<br />
<p>$_termsPageH2UseraccountsBody1()$</p>
<br />
<p>$_termsPageH2UseraccountsBody2()$</p>
<br />
<p>$_termsPageH2UseraccountsBody3()$</p>
<br />
<p>$_termsPageH2UseraccountsBody4()$</p>
<br />
<p>$_termsPageH2UseraccountsBody5()$</p>
<br />
<h2>$_termsPageH2Datahandeling()$</h2>
<br />
<p>$_termsPageH2DatahandelingBody1()$</p>
<br />
<p>$_termsPageH2DatahandelingBody2()$</p>
<br />
<p>$_termsPageH2DatahandelingBody3()$</p>
<br />
<p>$_termsPageH2DatahandelingBody4()$</p>
<br />
<h2>$_termsPageH2Signprocess()$</h2>
<br />
<p>$_termsPageH2SignprocessBody1()$</p>
<br />
<p>$_termsPageH2SignprocessBody2()$</p>
<br />
<p>$_termsPageH2SignprocessBody3()$</p>
<br />
<p>$_termsPageH2SignprocessBody4()$</p>
<br />
<p>$_termsPageH2SignprocessBody5()$</p>
<br />
<p>$_termsPageH2SignprocessBody6()$</p>
<br />
<p>$_termsPageH2SignprocessBody7()$</p>
<br />
<p>$_termsPageH2SignprocessBody8()$</p>
<br />
<h2>$_termsPageH2Communication()$</h2>
<br />
<p>$_termsPageH2CommunicationBody()$</p>
<br />
<h2>$_termsPageH2Ownerrights()$</h2>
<br />
<p>$_termsPageH2OwnerrightsBody1()$</p>
<br />
<p>$_termsPageH2OwnerrightsBody2()$</p>
<br />
<h2>$_termsPageH2Responsability()$</h2>
<br />
<p>$_termsPageH2ResponsabilityBody()$</p>
<br />
<h2>$_termsPageH2Law()$</h2>
<br />
<p>$_termsPageH2LawBody()$</p>
</div>
###
