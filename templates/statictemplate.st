####
staticTemplate=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html>
        <head>
                <title>SkrivaPå</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                <link href='http://fonts.googleapis.com/css?family=Copse' rel='stylesheet' type='text/css'>
                <link href="/css/main.css" rel="stylesheet" type="text/css" />
                <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.5.0/jquery.min.js" type="text/javascript"></script>
                <script src="/js/jquery.tweet.js" type="text/javascript"></script>
                <script src="/js/main.js" type="text/javascript"></script>
        </head>
        <body>
                <div id="top-slim-container">
                        <div class="content">
                                <a href="/" class="logo"></a>
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
                                                                </form>
                                                        </div>
                                                </div>
                                        </div>
                        </div>
                </div>
                <div id="body-container">
                        <div class="content">
                            $if(!nocolumns)$
                                <div class="left-column">
                                        <div class="white thin box">
                                                <h4>Hör av dig till oss!</h4>
                                                <br />
                                                <form method="post" action="$linkquestion$">
                                                        <input type="text" name="name" value="Namn" class="infotext" /><br />
                                                        <input type="email" name="email" value="E-post" class="infotext" /><br />
                                                        <input type="tel" name="phone" value="Telefonnummer" class="infotext" /><br />
                                                        <textarea name="message" class="infotext">Meddelande</textarea>
                                                        <a class="submit btn-small green2 center"><p>Skicka!</p></a>
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
                  $else$
                    $content:noescape()$
                  $endif$
                                <div class="clearfix"></div>
                                <div class="spacer40"></div>
                                <div class="try-skrivapa">
                                        <div class="left">
                                                <h2>6 månaders fri användning nu!</h2>
                                                <p>Allt inkluderat. Standardkonto, fria transaktioner och fri lagring.</p>
                                        </div>
                                        <div class="right">
                                                <form method="post" action="$linkrequestaccount$">
                                                        <input class="shiny infotext" name="email" type="email" value="Fyll i din e-postadress" /> <a href="#" class="submit btn-small green2 center float-right"><p>Skicka förfrågan!</p></a>
                                                </form>
                                        </div>
                                </div>
                        </div>
                        <div class="spacer40"></div>
                </div>
                <div id="footer-container">
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
                                                        info@skrivapa.se
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
        </body>
</html>
####
