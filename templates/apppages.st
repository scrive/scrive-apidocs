###
pageForgotPassword=
    <div class="centerdivnarrow">
      <form action=$LinkForgotPassword()$ method="post">
        <table>
          <tr>
            <td>E-mail</td>
            <td><input name="email" type="email"></td>
          </tr>
        </table>
        <input type="submit" value="Skicka nytt lösenord" class="validateEmail">
      </form>
    </div>

###
pageForgotPasswordConfirm=
    <div class="centerdivnarrow">
      <p>Ett nytt lösenord har skickats till din e-post. Du kan nu logga in med dina nya uppgifter.</p>
      $loginBox$
    </div>

###
pageLogin= 
    <div class="centerdivnarrow">
      <p class="headline">Logga in SkrivaPå!</p> 
      $loginBox()$
    </div>

###
wholePage=
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
    <html>
      <head>
        <title>$title$$if(production)$$else$ (devel) $endif$</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <link href='http://fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/newdesign.css" media="screen">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/style.css" media="screen">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/calendar.css" media="screen">
        <link rel="stylesheet" type="text/css" 
              href="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/ui-lightness/jquery-ui.css"
              media="screen">
        
        <script src="$protocol$//ajax.googleapis.com/ajax/libs/jquery/1.5.0/jquery.js"
                type="text/javascript"></script>
        <script src="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/jquery-ui.min.js"
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
        $facebooklogo()$
        $header()$
        <div id="mainContainer960">
          <div class="flashmsgbox">
            $flashmessages:{ msg | <div class="flashmessage">$msg:noescape()$</div> }$
          </div>
   
         
          $content:noescape()$
        </div>
      
   <div id="footer-container" class="newdesign">
                        <div class="top-border"></div>
                        <div class="spacer"></div>
                        <div class="content">
                                <div class="col left">
                                        <strong>TJÄNSTEN</strong>
                                        <ul>
                                                <li><a href="features.html">Funktioner</a></li>
                                                <li><a href="benefits.html">Fördelar</a></li>
                                                <li><a href="priceplan.html?nocolumns">Prisplan</a></li>
                                        </ul>
                                        <br />
                                        <strong>TRYGGHET OCH VILLKOR</strong>
                                        <ul>
                                                <li><a href="security.html">Säkerhet</a></li>
                                                <li><a href="legal.html">Juridik</a></li>
                                                <li><a href="privacypolicy.html">Sekretesspolicy</a></li>
                                                <li><a href="terms.html">SkrivaPå Allmäna Villkor</a></li>
                                        </ul>
                                        <br />
                                        <strong>SKRIVAPÅ</strong>
                                        <ul>
                                                <li><a href="about.html">Om SkrivaPå</a></li>
                                                <li><a href="partners.html">Partners</a></li>
                                                <li><a href="clients.html">Kunder</a></li>
                                                <li><a href="board.html">Board of Advisors</a></li>
                                        </ul>
                                </div>
                                <div class="col centre">
                                        <strong>SKRIVAPÅ PÅ TWITTER</strong>
                                        <div class="tweet">
                                        
                                        </div>
                                </div>
                                <div class="col right">
                                        <strong>Kontakta oss för feedback, frågor & förfrågningar</strong>
                                        <ul>
                                                <li>
                                                        <strong>Telefon:</strong>
                                                        010-195 99 15
                                                </li>
                                                <li>
                                                        <strong>Kontorstider:</strong>
                                                        måndag-fredag 09-17
                                                </li>
                                                <li>
                                                        <strong>Maila SkrivaPå:</strong>
                                                        <a href="mailto:info@skrivapa.se">info@skrivapa.se</a>
                                                </li>
                                                <li>
                                                        <strong>Adress:</strong>
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
        <input type="hidden" name="referer" value="$referer$">
        <table>
	  <tr>
            <td>E-mail:</td> 
            <td><input type="email" name="email" autocomplete="off" class="noflash" value="$if(email)$$email$$endif$"></td> 
          </tr>
	  <tr> 
            <td>Lösenord:</td> 
            <td><input type="password" name="password" autocomplete="off"></td> 
          </tr>
	  <tr> 
            <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"></td>
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
                                       <input type="email" autocomplete="off" name="invitedemail" infotext=" Bjud en vän på 2 månader fri användning. Skriv deras e-post här!" class="grayed">
                                       <a class="btn-tiny blue center float-right submit" href="#"><p>Skicka</p></a>
                                       <input type="submit" class="hidden" name="invite" value="Skicka">
                                  </div>
                                 </div>
                                 </form>
                                </span>
                                 
                                <a href="$linklogout$" class="logout-button"><p>Logga ut</p></a>
                                <a href="$linkaccount$" class="document-button"><p>Konto</p></a>
                                <a href="$linkissue$" class="document-button"><p>Arkiv</p></a>
                                <a href="$linkmain$" class="document-button"><p>Skapa</p></a>

                                $else$
                                <a href="#" class="login-button"><p>Logga in</p></a>
                                <div class="user-container">
                                                <div class="login-container">
                                                        <div class="top">
                                                                <p>Logga in</p>
                                                        </div>
                                                        <div class="body">
                                                                  <form method="post" action="$linklogin$">
                                                                        <input type="email" name="email" value="E-post" class="infotext" autocomplete="off"/><br />
                                                                        <input type="password" name="password" value="Lösenord" class="infotext" autocomplete="off"/><br />
                                                                        <a href="#" class="btn-tiny blue center float-right submit"><p>Logga in</p></a>
                                                                        <a href="#" class="txt-link">Glömt lösenord</a>
                                                                        <input type="submit" class="hidden">
                                                                  </form>
                                                        </div>
                                                </div>
                                                <div class="recovery-container">
                                                        <div class="top">
                                                                <p>Lösenord</p>
                                                        </div>
                                                        <div class="body">
                                                                <form method="post" action="$linkforgotenpassword$">
                                                                        <p class="float-left">&nbsp;Skicka nytt lösenord</p>
                                                                        <input type="email" name="email" value="E-post" class="infotext" autocomplete="off"/>
                                                                        <a href="#" class="btn-tiny blue center float-right submit"><p>Skicka</p></a>
                                                                        <a href="#" class="txt-link">Tillbaka</a>
                                                                        <input type="submit" class="hidden">
                                                                </form>
                                                        </div>
                                                </div>
                               $endif$
                            </div>
                           
                </div>

            <div class="clearboth"></div>
          </div>
####