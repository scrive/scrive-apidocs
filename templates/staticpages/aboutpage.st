###
aboutPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_aboutPageTitle()$</title>
        <meta name="description" content="$_aboutPagaMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBodyWithQueryCol(unwrappedbody=aboutPageContents())$
    </html>
###
aboutPageContents=
					<div class="opaque wide box" style="height:453px;">
                                              <div class="group-portrait-image">
                                                <a class="group"></a>
					      </div>
					</div>
					<br />
					<div class="opaque wide box smaller">
						<div class="about-container">
							<h1>$_aboutPageH1()$</h1>
							<br />
							$_aboutPageTopBody()$
							<ul class="partner-list">
								<li>
									<div class="portrait-image">
								          <a class="lukas"></a>
									</div>
									<h2>$_aboutPagePerson1()$</h2>
									<em>$_aboutPagePerson1Position()$</em>
									<p>$_aboutPagePerson1Body()$</p>
									$_aboutPagePerson1Contact()$
								</li>
								<li>
									<div class="portrait-image">
								          <a class="viktor"></a>
									</div>
									<h2>$_aboutPagePerson2()$</h2>
									<em>$_aboutPagePerson2Position()$</em>
									<p>$_aboutPagePerson2Body()$</p>
									$_aboutPagePerson2Contact()$
								</li>
								<li>
									<div class="portrait-image">
								          <a class="daniel"></a>
									</div>
									<h2>$_aboutPagePerson3()$</h2>
									<em>$_aboutPagePerson3Position()$</em>
									<p>$_aboutPagePerson3Body()$</p>
									$_aboutPagePerson3Contact()$
								</li>
								<li>
									<div class="portrait-image">
								          <a class="dudek"></a>
									</div>
									<h2>$_aboutPagePerson5()$</h2>
									<em>$_aboutPagePerson5Position()$</em>
									<p>$_aboutPagePerson5Body()$</p>
									$_aboutPagePerson5Contact()$

								</li>
                                                                <li>
                                                                        <div class="portrait-image">
                                                                          <a class="joakim"></a>
                                                                        </div>
                                                                        <h2>$_aboutPagePerson4()$</h2>
                                                                        <em>$_aboutPagePerson4Position()$</em>
                                                                        <p>$_aboutPagePerson4Body()$</p>
                                                                        $_aboutPagePerson4Contact()$
                                                                </li>
								<li>
									<div class="portrait-image">
								          <a class="gracjan"></a>
									</div>
									<h2>$_aboutPagePerson6()$</h2>
									<em>$_aboutPagePerson6Position()$</em>
									<p>$_aboutPagePerson6Body()$</p>
									$_aboutPagePerson6Contact()$
								</li>
								<li>
									<div class="portrait-image">
								          <a class="mariusz"></a>
									</div>
									<h2>$_aboutPagePerson7()$</h2>
									<em>$_aboutPagePerson7Position()$</em>
									<p>$_aboutPagePerson7Body()$</p>
									$_aboutPagePerson7Contact()$
								</li>
                                                                <li>
                                                                        <div class="portrait-image">
                                                                          <a class="magnus"></a>
                                                                        </div>
                                                                        <h2>$_aboutPagePerson8()$</h2>
                                                                        <em>$_aboutPagePerson8Position()$</em>
                                                                        <p>$_aboutPagePerson8Body()$</p>
                                                                        $_aboutPagePerson8Contact()$
                                                                </li>
                                                                <li>
                                                                        <div class="portrait-image">
                                                                          <a class="eric"></a>
                                                                        </div>
                                                                        <h2>$_aboutPagePerson9()$</h2>
                                                                        <em>$_aboutPagePerson9Position()$</em>
                                                                        <p>$_aboutPagePerson9Body()$</p>
                                                                        $_aboutPagePerson9Contact()$
                                                                </li>
                                                                <li>
                                                                        <div class="portrait-image">
                                                                          <a class="emily"></a>
                                                                        </div>
                                                                        <h2>$_aboutPagePerson10()$</h2>
                                                                        <em>$_aboutPagePerson10Position()$</em>
                                                                        <p>$_aboutPagePerson10Body()$</p>
                                                                        $_aboutPagePerson10Contact()$
                                                                </li>
                                                                <li>
                                                                        <div class="portrait-image">
                                                                          <a class="andrzej"></a>
                                                                        </div>
                                                                        <h2>$_aboutPagePerson11()$</h2>
                                                                        <em>$_aboutPagePerson11Position()$</em>
                                                                        <p>$_aboutPagePerson11Body()$</p>
                                                                        $_aboutPagePerson11Contact()$
                                                                </li>
                                                                <li>
                                                                        <div class="portrait-image">
                                                                          <a class="tim"></a>
                                                                        </div>
                                                                        <h2>$_aboutPagePerson12()$</h2>
                                                                        <em>$_aboutPagePerson12Position()$</em>
                                                                        <p>$_aboutPagePerson12Body()$</p>
                                                                        $_aboutPagePerson12Contact()$
                                                                </li>
							</ul>
						</div>
					</div>

<!-- Fix for image loading issue with week connection-->
<div class='image-reloader' style='display:none'>
        <img src='/img/team/group-2.jpg'/>
        <img src='/img/team/lukas-2.jpg'/>
        <img src='/img/team/viktor-2.jpg'/>
        <img src='/img/team/daniel-2.jpg'/>
        <img src='/img/team/marcus-2.png'/>
        <img src='/img/team/gracjan-2.jpg'/>
        <img src='/img/team/mariusz-2.jpg'/>
        <img src='/img/team/magnus-2.png'/>
        <img src='/img/team/eric-2.jpg'/>
        <img src='/img/team/emily-2.jpg'/>
        <img src='/img/team/andrzej-2.jpg'/>
        <img src='/img/team/tim-2.jpg'/>
</div>
###
