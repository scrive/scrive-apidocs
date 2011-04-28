###
partnersPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ | Partners</title>
        <meta name="description" content="Signera och arkivera dina avtal elektroniskt. Vi har nära samarbete med ett flertal framstående aktörer inom datasäkerhet och signeringsteknik." />
        $standardHeaderContents()$
      </head>
      <body>
         $bodystart()$
         <div class="left-column">
           $queryColumn()$
         </div>
         <div class="right-column">
           <div class="mainContainer">
             $partnersPageContents()$
           </div>
         </div>
         $bodyend()$ 
      </body>
    </html>
###
partnersPageContents=
					<div class="opaque wide box smaller">
						<h1>SkrivaPå´s Partners</h1>
						<p>SkrivaPå är en tjänst för att underteckna och arkivera dokument elektroniskt. Vi har ett nära samarbete med ett flertal framstående aktörer inom datasäkerhet och signeringsteknik.</p>
						<br />
						<ul class="partner-list">
							<li>
								<div class="partner-image">
									<img src="/img/partner-logica.png" />
								</div>
								<h2>Logica</h2>
								<em>Identifieringsteknik</em>
								<p>Logica är ett ledande internationellt IT-tjänstföretag som skapar värde genom att integrera människor, affärer och IT. Logica erbjuder verksamhetsinriktade konsulttjänster, systemintegration och outsourcing till kunder runt om i världen som inkluderar många av Europas största företag och organisationer.</p>
								<a href="http://www.logica.se">www.logica.se</a>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/partner-trustweaver.png" />
								</div>
								<div class="partner-info">
									<h2>Trustweaver</h2>
									<em>Dokumentsäkerhet och signering</em>
									<p>TrustWeaver är ett specialiserat IT-säkerhetsföretag som tillhandahåller e-signerings-, tidsstämplings-, arkivering- och fakturainnehållslösningar som gör det möjligt för företag att vara 100% säkra att deras e-fakturor och andra företagskritiska elektroniska dokument godkänns av kontrollinstitut och domstolar som fullständiga och oändrade över väldigt långa tidsperioder.</p>
									<a href="http://www.trustweaver.com">www.trustweaver.com</a>
								</div>
							</li>
							<li>
								<div class="partner-image">
									<img src="/img/partner-chalmers.png" />
								</div>
								<h2>Chalmers</h2>
								<em>Datasäkerhet</em>
								<p>Chalmers är ett av världens mest prestigefyllda tekniska universitet. SkrivaPå har ett nära samarbete med Chalmers Datasäkerhetsgrupp och samverkar i framtagningen av morgondagens säkerhetslösningar för så kallade cloud tjänster. </p>
								<a href="http://www.chalmers.se">www.chalmers.se</a>
							</li>
						</ul>
					</div>
###
