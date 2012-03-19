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
<h1>$_termsPageHeader()$</h1>
<br />
<p>$_termsPage0()$</p>
<br />
<h2>$_termsPage1Header()$</h2>
<br />
<p>$_termsPage1()$</p>
<br />
<p>$_termsPage1a()$</p>
<br />
<p>$_termsPage1b()$</p>
<br />
<p>$_termsPage1c()$</p>
<br />
<p>$_termsPage1d()$</p>
<br />
<h2>$_termsPage2Header()$</h2>
<br />
<p>$_termsPage2()$</p>
<br />
<p>$_termsPage2a()$</p>
<br />
<p>$_termsPage2b()$</p>
<br />
<p>$_termsPage2c()$</p>
<br />
<h2>$_termsPage3Header()$</h2>
<br />
<p>$_termsPage3()$</p>
<br />
<p>$_termsPage3a()$</p>
<br />
<p>$_termsPage3b()$</p>
<br />
<p>$_termsPage3c()$</p>
<br />
<p>$_termsPage3d()$</p>
<br />
<h2>$_termsPage4Header()$</h2>
<br />
<p>$_termsPage4()$</p>
<br />
<h2>$_termsPage5Header()$</h2>
<br />
<p>$_termsPage5()$</h2>
<br />
<p>$_termsPage5a()$</p>
<br />
<h2>$_termsPage6Header()$</h2>
<br />
<p>$_termsPage6()$</p>
<br />
<h2>$_termsPage7Header()$</h2>
<br />
<p>$_termsPage7a()$</p>
<br />
<p>$_termsPage7b()$</p>
<br />
<h2>$_termsPage8Header()$</h2>
<br />
<p>$_termsPage8()$</p>
</div>
###
