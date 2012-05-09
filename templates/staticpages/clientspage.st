###
clientsPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_clientsPageTitle()$</title>
        <meta name="description" content="Signera och arkivera dina avtal elektroniskt. Våra kunder och våra kunders kunder älskar enkelheten." />
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBodyWithQueryCol(unwrappedbody=clientsPageContents())$
    </html>
###
clientsPageContents=
					<div class="opaque wide box smaller">
						<h1>$_clientsPageH1()$</h1>
						$_clientsPageTopBody()$
						<br />
						<ul class="partner-list">
							<li>
								<div class="partner-image">
									<img src="/img/goenterprise-logo.png" />
								</div>
								<h2>$_clientsPageCusomerVid()$</h2>
								<blockquote>$_QuoteOnVid()$</blockquote>
								<em>$_QuoteOnVidBy()$</em>
								<p>$_clientsPageCusomerVidAbout()$</p>
								<a href="http://www.goenterprise.se">$_clientsPageCusomerVidWeb()$</a>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/accountfactory-logo.png" style="left:3px;top:3px;"/>
								</div>
								<h2>$_clientsPageCusomer1()$</h2>
								<blockquote>$_Quote1()$</blockquote>
								<em>$_Quote1By()$</em>
								<p>$_clientsPageCusomer1About()$</p>
								<a href="http://www.accountfactory.com">$_clientsPageCusomer1Web()$</a>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/greenhatpeople-logo.png" />
								</div>
								<h2>$_clientsPageCusomer2()$</h2>
								<blockquote>$_Quote2()$</blockquote>
								<em>$_Quote2By()$</em>
								<p>$_clientsPageCusomer2About()$</p>
								<a href="http://www.greenhatpeople.se">$_clientsPageCusomer2Web()$</a>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/eventbook-logo.png" />
								</div>
								<h2>$_clientsPageCusomer3()$</h2>
								<blockquote>$_Quote3()$</blockquote>
								<em>$_Quote3By()$</em>
								<p>$_clientsPageCusomer3About()$</p>
								<a href="http://www.eventbook.se">$_clientsPageCusomer3Web()$</a>
							</li>
                                                        <li>
								<div class="partner-image">
									<img src="/img/accenture-logo.png" />
								</div>
								<h2>$_clientsPageCusomer4()$</h2>
								<blockquote>$_Quote4()$</blockquote>
								<em>$_Quote4By()$</em>
								<p>$_clientsPageCusomer4About()$</p>
								<a href="http://www.accenture.com/se-en/Pages/index.aspx">$_clientsPageCusomer4Web()$</a>
							</li>
                                                        <li>
								<div class="partner-image">
									<img src="/img/coop-logo.png" />
								</div>
								<h2>$_clientsPageCusomer5()$</h2>
								<blockquote>$_Quote5()$</blockquote>
								<em>$_Quote5By()$</em>
								<p>$_clientsPageCusomer5About()$</p>
								<a href="http://www.coop.se/coopnord">$_clientsPageCusomer5Web()$</a>
							</li>                                                        <li>
								<div class="partner-image">
									<img src="/img/ipas-logo.gif"  style="padding:68px 8px;background-color:black;"/>
								</div>
								<h2>$_clientsPageCusomer6()$</h2>
								<blockquote>$_Quote6()$</blockquote>
								<em>$_Quote6By()$</em>
								<p>$_clientsPageCusomer6About()$</p>
								<a href="http://www.ipas.se/">$_clientsPageCusomer6Web()$</a>
							</li>
						</ul>
					</div>
###
