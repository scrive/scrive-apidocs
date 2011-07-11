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
<h1>$_privacyPolicyPageH1()$</h1>
<br />
$_privacyPolicyPageH1Body()$
<br />
<h2>$_privacyPolicyPageH21()$</h2>
<br />
<p>$_privacyPolicyPageH21Body()$</p>
<br />
<h2>$_privacyPolicyPageH23()$</h2>
<br />
<p>$_privacyPolicyPageH23Body()$</p>
<br />
<h2>$_privacyPolicyPageH24()$</h2>
<br />
<p>$_privacyPolicyPageH24Body()$</p>
</div>
###
