###
securityPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_securityPageTitle()$</title>
        <meta name="description" content="$_securityPageMetaDescription()$" />
        $standardHeaderContents()$
      </head>
      $standardBodyWithQueryCol(unwrappedbody=securityPageContents())$
    </html>
###
securityPageContents=
<div class="nicetext">
<h1>$_securityPageH1()$</h1>
<br />
<h2>$_securityPageH2Intro()$</h2>
<br />
<p>$_securityPageH2IntroBody1()$</p>
<br />
<ol class="numbers">
	$_securityPageH2IntroBody2()$</ol>
<br />
<p>$_securityPageH2IntroBody3()$</p>
<br />
<ul class="discs">
	$_securityPageH2IntroBody4()$</ul>
<br />
<h2>$_securityPageH2Haskell()$</h2>
<br />
<p>$_securityPageH2HaskellBody()$</p>
<br />
<h2>$_securityPageH2SSL()$</h2>
<br />
<p>$_securityPageH2SSLBody()$</p>
<br />
<h2>$_securityPageH2Internal()$</h2>
<br />
<p>$_securityPageH2InternalBody()$</p>
<br />
<h2>$_securityPageH2Change()$</h2>
<br />
<p>$_securityPageH2ChangeBody()$</p>
<br />
<h2>$_securityPageH2Staff()$</h2>
<br />
<p>$_securityPageH2StaffBody()$</p>
<br />
<h2>$_securityPageH2Chalmers()$</h2>
<br />
<p>$_securityPageH2ChalmersBody()$</p>
</div>
<script>
  mixpanel.track('View Security Page');
</script>

###
