###
partnersPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_partnersPageTitle()$</title>
        <meta name="description" content="$_partnersPageMetaDescription()$" />
        $standardHeaderContents()$
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
<script>
  mixpanel.track('View Partners Page');
</script>

###
