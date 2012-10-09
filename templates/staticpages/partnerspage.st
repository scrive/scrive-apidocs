###
partnersPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_partnersPageTitle()$</title>
        <meta name="description" content="$_partnersPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBodyWithQueryCol(unwrappedbody=partnersPageContents())$
    </html>
###
partnersPageContents=
					<div class="opaque wide box smaller">
						<h1>$_partnersPageH1()$</h1>
						<p>$_partnersPageH1Body()$</p>
						<br />
						<ul class="partner-list">
							<li>
								<div class="partner-image">
									<img src="/img/partner-logica.png" />
								</div>
								<h2>$_partnersPagePartner1()$</h2>
								<em>$_partnersPagePartner1Area()$</em>
								<p>$_partnersPagePartner1About()$</p>
								<a href="http://www.logica.se">$_partnersPagePartner1Web()$</a>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/logo/guardtime_logo.png" />
								</div>
								<div class="partner-info">
									<h2>$_partnersPagePartner2()$</h2>
									<em>$_partnersPagePartner2Area()$</em>
									<p>$_partnersPagePartner2About()$</p>
									<a href="http://www.guardtime.com">$_partnersPagePartner2Web()$</a>
								</div>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/partner-chalmers.png" />
								</div>
								<h2>$_partnersPagePartner3()$</h2>
								<em>$_partnersPagePartner3Area()$</em>
								<p>$_partnersPagePartner3About()$</p>
								<a href="http://www.chalmers.se">$_partnersPagePartner3Web()$</a>
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
