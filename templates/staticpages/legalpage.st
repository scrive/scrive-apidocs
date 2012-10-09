###
legalPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_legalPageTitle()$</title>
        <meta name="description" content="$_legalPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBodyWithQueryCol(unwrappedbody=legalPageContents())$
    </html>
###
legalPageContents=
					<div class="nicetext">
<h1>$_legalPageH1()$</h1>
<br />
<h2>$_legalPageH2Intro()$</h2>
<br />
<p>$_legalPageH2IntroBody()$</p>
<br />
<h3>$_legalPageH2IntroH31()$</h3>
<br />
<p>$_legalPageH2IntroH31Body1()$</p>
<br />
<p>$_legalPageH2IntroH31Body2()$</p>
<br />
<p>$_legalPageH2IntroH31Body3()$</p>
<br />
<h3>$_legalPageH2IntroH32()$</h3>
<br />
<p>$_legalPageH2IntroH32Body()$</p>
<br />
<h3>$_legalPageH2IntroH33()$</h3>
<br />
<p>$_legalPageH2IntroH33Body1()$</p>
<br />
<p>$_legalPageH2IntroH33Body2()$</p>
<br />
<ol class="letters">
	$_legalPageH2IntroH33Body3()$</ol>
<br />
<br />
<h2>$_legalPageH2KeyLegal()$</h2>
<br />
<p>$_legalPageH2KeyLegalBody1()$</p>
<br />
<ol class="letters">
	$_legalPageH2KeyLegalBody2()$
</ol>
<br />
<h3>$_legalPageH2KeyLegalH31()$</h3>
<br />
<p>$_legalPageH2KeyLegalH31Body()$</p>
<br />
<h3>$_legalPageH2KeyLegalH32()$</h3>
<br />
<p>$_legalPageH2KeyLegalH32Body1()$</p>
<br />
<p>$_legalPageH2KeyLegalH32Body2()$</p>
<br />
<p>$_legalPageH2KeyLegalH32Body3()$</p>
<br />
<ol class="numbers">$_legalPageH2KeyLegalH32Body31()$
	</ol>
<br />
<p>$_legalPageH2KeyLegalH32Body4()$</p>
<br />
<h3>$_legalPageH2KeyLegalH33()$</h3>
<br />
$_legalPageH2KeyLegalH33Body1()$
<br />
<ol class="numbers">
	$_legalPageH2KeyLegalH33Body2()$</ol>
<br />
<p>$_legalPageH2KeyLegalH33Body3()$</p>
<br />
<br />
<h3>$_legalPageH2KeyLegalH34()$</h3>
<br />
<p>$_legalPageH2KeyLegalH34Body()$</p>
<br />
<br />
<h2>$_legalPageH2SignProcess()$</h2>
<br />
<h3>$_legalPageH2SignProcessH31()$</h3>
<br />
<p>$_legalPageH2SignProcessH31Body1()$</p>
<br />
<table>
	<tr>$_legalPageH2SignProcessH31Body2TableHead()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table1()$</tr>
	<tr>$_legalPageH2SignProcessH31Body2Table2()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table3()$</tr>
	<tr>$_legalPageH2SignProcessH31Body2Table4()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table5()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table6()$</tr>
	<tr>$_legalPageH2SignProcessH31Body2Table7()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table8()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table9()$</tr>
	<tr>$_legalPageH2SignProcessH31Body2Table10()$</tr>
	<tr class="odd">$_legalPageH2SignProcessH31Body2Table11()$</tr>
	<tr>$_legalPageH2SignProcessH31Body2Table12()$</tr>
</table>
<br />
<p>$_legalPageH2SignProcessH31Body3()$</p>
<br />
<ol class="letters">
	$_legalPageH2SignProcessH31Body4()$
</ol>
<br />
<br />
<h2>$_legalPageH2Proof()$</h2>
<br />
<h3>$_legalPageH2ProofH31()$</h3>
<p>$_legalPageH2ProofH31Body1()$</p>
<br />
<p>$_legalPageH2ProofH31Body2()$</p>
<br />
<p>$_legalPageH2ProofH31Body3()$</p>
<br />
<p>$_legalPageH2ProofH31Body4()$</p>
<br />
<p>$_legalPageH2ProofH31Body5()$</p>
<br />
<ol class="numbers">
	$_legalPageH2ProofH31Body6()$</ol>
<br />
<h3>$_legalPageH2ProofH32()$</h3>
<br />
<p>$_legalPageH2ProofH32Body1()$</p>
<br />
<ol class="numbers">
	$_legalPageH2ProofH32Body2()$</ol>
<br />
<br />
<h2>$_legalPageH2DocHandeling()$</h2>
<br />
<h3>$_legalPageH2DocHandelingH31()$</h3>
<br />
<p>$_legalPageH2DocHandelingH31Body1()$</p>
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
