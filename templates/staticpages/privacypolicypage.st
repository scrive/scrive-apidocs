###
privacyPolicyPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_privacyPolicyPageTitle()$</title>
        <meta name="description" content="$_privacyPolicyPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBodyWithQueryCol(unwrappedbody=privacyPolicyPageContents())$
    </html>
###
privacyPolicyPageContents=
<div class="nicetext">
<h1>$_privacyPolicyPageHeader()$</h1>
<br />
<p>$_privacyPolicyPageP1()$</p>
<br />
<p>$_privacyPolicyPageP2()$</p>
<br />
<h2>$_privacyPolicyPage1Header()$</h2>
<br />
<p>$_privacyPolicyPage1()$</p>
<br />
<h2>$_privacyPolicyPage2Header()$</h2>
<br />
<p>$_privacyPolicyPage2()$</p>
<br />
<h2>$_privacyPolicyPage3Header()$</h2>
<br />
<p>$_privacyPolicyPage3()$</p>
<br />
<h2>$_privacyPolicyPage4Header()$</h2>
<br />
<p>$_privacyPolicyPage4()$</p>
<br />
<h2>$_privacyPolicyPage5Header()$</h2>
<br />
<p>$_privacyPolicyPage5()$</p>
</div>
###
