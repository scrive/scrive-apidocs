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
									<div class="go-enterprise"></div>
								</div>
								<h2>$_clientsPageCusomer0()$</h2>
								<blockquote>$_Quote0()$</blockquote>
								<em>$_Quote0By()$</em>
								<p>$_clientsPageCusomer0About()$</p>
								<a href="http://www.avanza.se" class='home-link'>$_clientsPageCusomer0Web()$</a>
							</li>
							<li>
								<div class="partner-image">
                                                                        <div class="account-factory"></div>
								</div>
								<h2>$_clientsPageCusomer1()$</h2>
								<blockquote>$_Quote1()$</blockquote>
								<em>$_Quote1By()$</em>
								<p>$_clientsPageCusomer1About()$</p>
								<a href="http://www.hemnet.se" class='home-link'>$_clientsPageCusomer1Web()$</a>
							</li>
							<li>
								<div class="partner-image">
                                                                        <div class="green-hat-people"></div>
								</div>
								<h2>$_clientsPageCusomer2()$</h2>
								<blockquote>$_Quote2()$</blockquote>
								<em>$_Quote2By()$</em>
								<p>$_clientsPageCusomer2About()$</p>
								<a href="http://www.mynewsdesk.com" class='home-link'>$_clientsPageCusomer2Web()$</a>
							</li>
							<li>
								<div class="partner-image">
                                                                        <div class="event-book"></div>
								</div>
								<h2>$_clientsPageCusomer3()$</h2>
								<blockquote>$_Quote3()$</blockquote>
								<em>$_Quote3By()$</em>
								<p>$_clientsPageCusomer3About()$</p>
								<a href="http://www.coop.se/coopnord" class='home-link'>$_clientsPageCusomer3Web()$</a>
							</li>
                                                        <li>
								<div class="partner-image">
                                                                        <div class="accenture"></div>
								</div>
								<h2>$_clientsPageCusomer4()$</h2>
								<p>$_clientsPageCusomer4About()$</p>
								<a href="http://peaceandlove.se" class='home-link'>$_clientsPageCusomer4Web()$</a>
							</li>
						</ul>
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
