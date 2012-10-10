###
privacyPolicyPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_privacyPolicyPageTitle()$</title>
        <meta name="description" content="$_privacyPolicyPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
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
        $if(production)$
        <!-- Snoobi code tracking code -->
            <script type="text/javascript">
            var psSite = "8d72086524";
            var peJsHost = "https://";
            document.write(unescape("%3Cscript src='https://tr.prospecteye.com/track.js' type='text/javascript'%3E%3C/script%3E"));
            </script>
            <noscript><img src="https://tr.prospecteye.com/?id=8d72086524" style="border:0;display:none;"/></noscript>
       <!-- End of snoobi -->
       $endif$

###
