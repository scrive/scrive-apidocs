###
pageLogin= 
    <div class="centerdivnarrow">
      <p class="headline">Logga in</p> 
      $loginBox()$
    </div>

###
wholePage=
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
    <html>
      <head>
        <title>$title$$if(production)$$else$ (devel) $endif$</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        $facebooklogo()$
        <link href='$protocol$//fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/newdesign.css" media="screen">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/style.css" media="screen">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/calendar.css" media="screen">
		<link rel="stylesheet" type="text/css" href="$prefix$/theme/symbols.css" media="screen">
		<link rel="stylesheet" type="text/css" href="$prefix$/theme/buttons.css" media="screen">
        <link href="$prefix$/theme/flash.css" rel="stylesheet" type="text/css" media="screen" />
        <link href="/css/main.css" rel="stylesheet" type="text/css" />
        <link rel="stylesheet" type="text/css"  href="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.11/themes/ui-lightness/jquery-ui.css"              media="screen" />
        <script src="$protocol$//ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"
                type="text/javascript"></script>
        <script src="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.11/jquery-ui.min.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/jquery.tools.min.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/jquery.MultiFile.js"
                type="text/javascript"></script>
        <script src="$prefix$/tiny_mce/jquery.tinymce.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/global.js"
                type="text/javascript"></script>
       <script src="/js/jquery.tweet.js" 
                type="text/javascript"></script>
       <script src="/js/main.js" 
                type="text/javascript"></script>
      </head>
      <body>
        $header()$
        <div class="mainContainer960" >
          $flashmessages()$
          $loginModal()$
        <div id="body-container">
         $if(!columns)$
          $content:noescape()$
         $else$
         <div class="left-column">
                                        <div class="white thin box">
                                                <h4>Hör av dig till oss!</h4>
                                                <br />
                                                <form method="post" action="$linkquestion$">
                                                        <input type="text" name="name" infotext="Namn" /><br />
                                                        <input type="email" name="email" infotext="E-post" /><br />
                                                        <input type="tel" name="phone" infotext="Telefonnummer" /><br />
                                                        <textarea name="message" infotext="Meddelande"></textarea>
                                                        <a class="submit new-btn-small green2 center"><p>Skicka!</p></a>
                                                        <input type="submit" class="hidden" />
                                                </form>
                                                <div class="hor-divider"></div>
                                                <h4>SkrivaPå AB</h4>
                                                <ul class="info-list">
                                                        <li>Saltmätargatan 19a, 4 tr<br />
                                                        113 59 Stockholm</li>
                                                        <li>Telefon: 020 12 11 10<br />E-post: info@skrivapa.se</li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="right-column">
                                           $content:noescape()$
                                </div>
         $endif$ 
          <div class="clearfix"></div>
          <div class="spacer40"></div>
                $if(showCreateAccount)$    
                 <div class="try-skrivapa">
                     <div class="left">
                        <h2>2 månaders fri användning nu!</h2>
                        <p>Allt inkluderat. Standardkonto, fria transaktioner och fri lagring.</p>
                        </div>
                         <div class="right">
                            <form method="post" action="$linkrequestaccount$" class="requestAccount">
                                   <input class="shiny" name="email" type="email" infotext="Fyll i din e-postadress" /> <a href="#" class="submit new-btn-small green2 center float-right"><p>Skicka förfrågan!</p></a>
                                   <input type="submit" class="hidden" />
                             </form>
                         </div>
                    </div>
               $endif$ 
         </div>
       </div>
                
   <div id="footer-container" class="newdesign">
                        <div class="top-border"></div>
                        <div class="spacer"></div>
                        <div class="content">
                                <div class="col left">
                                        <strong>TJÄNSTEN</strong>
                                        <ul>
                                                <li><a href="/features.html?columns">Funktioner</a></li>
                                                <li><a href="/benefits.html?columns">Fördelar</a></li>
                                                <li><a href="/priceplan.html">Prisplan</a></li>
                                        </ul>
                                        <br />
                                        <strong>TRYGGHET OCH VILLKOR</strong>
                                        <ul>
                                                <li><a href="/security.html?columns">Säkerhet</a></li>
                                                <li><a href="/legal.html?columns">Juridik</a></li>
                                                <li><a href="/privacypolicy.html?columns">Sekretesspolicy</a></li>
                                                <li><a href="/terms.html?columns">SkrivaPå Allmäna Villkor</a></li>
                                        </ul>
                                        <br />
                                        <strong>SKRIVAPÅ</strong>
                                        <ul>
                                                <li><a href="/about.html?columns">Om SkrivaPå</a></li>
                                                <li><a href="/partners.html?columns">Partners</a></li>
                                                <li><a href="/clients.html?columns">Kunder</a></li>
                                                <li><a href="/board.html?columns">Board of Advisors</a></li>
                                        </ul>
                                </div>
                                <div class="col centre">
                                        <strong>SKRIVAPÅ PÅ TWITTER</strong>
                                        <div class="tweet">
                                        
                                        </div>
                                </div>
                                <div class="col right">
                                        <strong>Kontakta oss för feedback, frågor &amp; förfrågningar</strong>
                                        <ul>
                                                <li>
                                                        <strong>Telefon</strong>
                                                        010-195 99 15
                                                </li>
                                                <li>
                                                        <strong>Kontorstider</strong>
                                                        måndag-fredag 09-17
                                                </li>
                                                <li>
                                                        <strong>Maila SkrivaPå</strong>
                                                        <a href="mailto:info@skrivapa.se">info@skrivapa.se</a>
                                                </li>
                                                <li>
                                                        <strong>Adress</strong>
                                                        SkrivaPå CM AB<br />
                                                        Saltmätargatan 19A<br />
                                                        113 59 Stockholm
                                                </li>
                                        </ul>
                                </div>
                        </div>
        </div>
   
        $if(production)$
        <script type="text/javascript">
          var _gaq = _gaq || [];
          _gaq.push(['_setAccount', 'UA-6387711-9']);
          _gaq.push(['_trackPageview']);

          (function() {
          var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
          })();

        </script>
        <script type="text/javascript" src="https://eu1.snoobi.com/snoop.php?tili=skrivapa_se"></script>
        $endif$
      </body>
    </html>

####
loginBox=
    <div id="login">
      <form action="/login" method="post">
        <input type="hidden" name="referer" value="$referer$" />
        <table>
	  <tr>
            <td>E-post:</td> 
            <td><input type="email" name="email" autocomplete="off" class="noflash" value="$if(email)$$email$$endif$" /></td> 
          </tr>
	  <tr> 
            <td>Lösenord:</td> 
            <td><input type="password" name="password" autocomplete="off" /></td> 
          </tr>
	  <tr> 
            <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in" /></td>
            <td><a href="/amnesia"> Glömt lösenord</a></td>
	  </tr>
        </table>
      </form>
    </div>

####
header=
 <div id="top-slim-container" class="newdesign">
                        <div class="content">
                                <a href="/" class="logo"></a>
                                $if(logged)$
                               <span id="inviteContainer">
                                 <form method="post" action="$linkinvite$" class="MultiFile-intercepted">
                                 <div>
                                  <div>
                                       <input type="email" class="float-left" autocomplete="off" name="invitedemail" infotext=" Bjud en vän på 2 månader fri användning. Skriv deras e-post här!"/>
                                       <a class="btn-tiny green float-right submit" href="#">
                                            <div class="left"></div>
                                            <div class="label"><p>Bjud in</p></div>
                                            <div class="right"></div> 
                                       </a>
                                       <input type="submit" class="hidden" name="invite" value="Skicka" />
                                  </div>
                                 </div>
                                 </form>
                                </span>
                                 
                                <a href="$linklogout$" class="document-button"><p>Logga ut</p></a>
                                <a href="$linkaccount$" class="document-button"><p>Konto</p></a>
                                <a href="$linkissue$" class="document-button"><p>Arkiv</p></a>
                                <a href="$linkmain$" class="document-button"><p>Skicka</p></a>

                                $else$
                                <a href="#" class="login-button"><p>Logga in</p></a>
                                $endif$
                            </div>
                           
            <div class="clearboth"></div>
          </div>
####
