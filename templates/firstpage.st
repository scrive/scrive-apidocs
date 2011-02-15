####
firstPage=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
        <head>
                <title>SkrivaPå</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                $facebooklogo()$
                <link href='http://fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css">
                <link href="/css/main.css" rel="stylesheet" type="text/css" />
                <link href="/theme/flash.css" rel="stylesheet" type="text/css" />
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
                <script src="/js/jquery.tweet.js" type="text/javascript"></script>
                <script src="/js/main.js" type="text/javascript"></script>
                <script type="text/javascript">
                        function repeatSlide() {
                                doSlide('.slide-content');
                        }
                        
                        function doSlide(elm) {
                                var items = jQuery(elm).length;
                                var current = jQuery(elm + '.active').index()-1;
                                
                                if(current == (items-1)) {
                                        jQuery(elm).eq(current).fadeOut().removeClass('active');
                                        jQuery(elm).eq(0).fadeIn().addClass('active');
                                } else {
                                        jQuery(elm).eq(current).fadeOut('slow').removeClass('active');
                                        jQuery(elm).eq(current+1).fadeIn('slow').addClass('active');
                                }
                                
                                jQuery('.slide-nav ul li').removeClass('active').eq(jQuery(elm + '.active').index()-1).addClass('active');
                                
                                jQuery.data(this, 'slide', setTimeout( function() {
                                        repeatSlide();
                                }, 10000));
                        }
                        
                        function gotoPage(elm, index) {
                                var items = jQuery(elm).length;
                                
                                if(index < items) {
                                        clearTimeout(jQuery.data(this, 'slide'));
                                        
                                        jQuery(elm).filter('.active').fadeOut().removeClass('active');
                                        jQuery('.slide-nav ul li').removeClass('active');
                                        
                                        jQuery(elm).eq(index).fadeIn().addClass('active');
                                        jQuery('.slide-nav ul li').eq(index).addClass('active');
                                        
                                        jQuery.data(this, 'slide', setTimeout( function() {
                                                repeatSlide();
                                        }, 10000));
                                }
                        }
                        
                        jQuery(document).ready( function() {
                                jQuery('.campaign-text a, .campaign-play, #nav-container .col a').click( function(e) {
                                        e.preventDefault();
                                        
                                        var html = '<object> <embed width="590" height="370" allowscriptaccess="always" allowfullscreen="true" type="application/x-shockwave-flash" src="https://secure.vimeo.com/moogaloop.swf?clip_id=19914678&amp;server=vimeo.com&amp;show_title=0&amp;show_byline=0&amp;show_portrait=0&amp;color=00ADEF&amp;fullscreen=1"></object>';
                                        
                                        openModal(html, {
                                                width: '630',
                                                height: '540',
                                                topTxt: '<h3>Demonstration</h3>Följande är ett demonstrationsklipp av hur man undertecknar ett dokument med vår tjänst.'
                                        });
                                });
                                
                                jQuery('.slide-nav ul li').click( function() {
                                        var index = jQuery(this).index();
                                        
                                        gotoPage('.slide-content', index);
                                });
                                
                                doSlide('.slide-content');
                        });
                </script>
        </head>
        <body>
                <div id="wrap">
                $flashmessages()$
                <div id="top-container">
                        <div class="content">
                                <div class="logo no-txt">SkrivaPå</div>
                                <div class="left">
                                        <div class="intro-container">
                                                <h2>Snabbt. Smidigt. Säkert.</h2>
                                                <h3>SkrivaPå är en tjänst för att skriva på och arkivera avtal elektroniskt. Minska kostnaderna, öka omsättningen och spar på frustrationen. Välkommen till framtiden!
                                                <br />
                                                <br />
                                                <br />
                                                </h3>
                                                <em>Vi bjuder på 2 månaders fri användning nu!</em>
                                                <form method="post" action="$linkrequestaccount$" class="requestAccount">
                                                        <input class="shiny infotext" type="email" name="email" value="Fyll i din e-postadress" /> <a href="#" class="submit btn-small green2 center"><p>Skicka förfrågan!</p></a>
                                                        <input type="submit" class="hidden">
                                                </form>
                                        </div>
                                </div>
                                <div class="right">
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
                                        </div>
                                        <div class="campaign-container">
                                                <div class="campaign-play"></div>
                                                <div class="campaign-text">
                                                        <blockquote>”Processerna blir betydligt smidigare. Man sparar tid helt enkelt.”</blockquote>
                                                        <em>- Lisa Renander och Julia Hamrin, VD och Säljchef på Go Enterprise, Management Consulting</em>
                                                        <a href="#">Se demo</a>
                                                </div>
                                        </div>
                                </div>
                                <div class="person"></div>
                        </div>
                </div>
                <div id="nav-container">
                        <div class="content">
                                <div class="col left">
                                        <div class="icon one"></div>
                                        <a href="#"><h4>Skicka</h4>
                                        <p>Ladda upp dokument,<br />välj mottagare, skicka!</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col centre">
                                        <div class="icon two"></div>
                                        <a href="#"><h4>Skriva på</h4>
                                        <p>Alla parter skriver på<br />elektroniskt.</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col right">
                                        <div class="icon three"></div>
                                        <a href="#"><h4>Spara och arkivera</h4>
                                        <p>Vi sparar dina dokument<br />tryggt och säkert.</p></a>
                                </div>
                        </div>
                        <div class="bottom-border"></div>
                </div>
                <div id="body-container">
                        <div class="content">
                                <div class="spacer50"></div>
                                <div class="grid 4x4">
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon money"></div>
                                                </div>
                                                <a href="#"><h5>Minska kostnaderna</h5>
                                                <p>Dokumenthantering är tidskrävande. Tid är pengar. SkrivaPå är ett skräddarsytt redskap som optimerar dina processer. Spara 50-250kr/dokument. </p></a>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon watch"></div>
                                                </div>
                                                <a href="#"><h5>Snabbare avslut</h5>
                                                <p>Snabbare avslut leder till fler avslut. Vi hjälper dig att öka din omsättning.</p></a>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon globe"></div>
                                                </div>
                                                <a href="#"><h5>Överallt. Alltid.</h5>
                                                <p>Arbeta när du vill och var du vill. Glöm problemen med att buda dokument jorden runt.</p></a>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon seal"></div>
                                                </div>
                                                <a href="#"><h5>Starkare bevis</h5>
                                                <p>SkrivaPå verifierar transaktionen och stärker din bevisposition vid konflikt. Vi ger dig svårmatchad juridisk tyngd.</p></a>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon safe"></div>
                                                </div>
                                                <a href="#"><h5>100% kontroll</h5>
                                                <p>Vår säkra lagring garanterar dokumentens integritet. Äntligen full kontroll. </p></a>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon face"></div>
                                                </div>
                                                <a href="#"><h5>Minska frustrationen</h5>
                                                <p>Det är sällan roligt att vända papper, leta efter förlorade dokument och jaga signaturer. SkrivaPå lättar din och dina kunders administrativa börda.</p></a>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon paper"></div>
                                                </div>
                                                <a href="#"><h5>Spara på miljön</h5>
                                                <p>Visa att ni tar ansvar. Skippa pappersavtalandet och minska din miljöpåverkan.</p></a>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon ipad"></div>
                                                </div>
                                                <a href="#"><h5>Image branding</h5>
                                                <p>Visa era kunder att ni är i framkant och gillar att tänka i nya banor.</p></a>
                                        </div>
                                </div>
                                <div class="clearfix"></div>
                                <div class="slide">
                                        <div class="slide-container">
                                                <h3>Kunder berättar</h3>
                                                <div class="slide-content active">
                                                        <img src="/img/about-lukas-d.png" height="142" width="122" class="portrait" />
                                                        <blockquote>”Skönt. Snabbare vändor med kund och man slipper arkivera papper.”</blockquote>
                                                        <em>Mikael Green, VD, Account Factory, Redovisningsbyrå</em>
                                                </div>
                                                <div class="slide-content">
                                                        <img src="/img/about-lukas-d.png" height="142" width="122" class="portrait" />
                                                        <blockquote>”Man har mycket bättre koll på de avtal som ingåtts. Det är grejen tycker jag.”</blockquote>
                                                        <em>Alexis Barnekow, Försäljningschef, Green Hat People, Teambuilding Events</em>
                                                </div>
                                                <div class="slide-content">
                                                        <img src="/img/about-lukas-d.png" height="142" width="122" class="portrait" />
                                                        <blockquote>”Smart lösning.”</blockquote>
                                                        <em>Christoffer Lindquist, VD, Eventbook, Eventpartner</em>
                                                </div>
                                        </div>
                                        <div class="slide-nav">
                                                <ul>
                                                        <li class="active"></li>
                                                        <li></li>
                                                        <li></li>
                                                </ul>
                                        </div>
                                        <div class="bubble">
                                                <h3>Partners</h3>
                                                <ul>
                                                        <li><a href="partners.html"><img src="/img/partner-trustweaver.png"  /></a></li>
                                                        <li><a href="partners.html"><img src="/img/partner-logica.png" /></a></li>
                                                        <li><a href="partners.html"><img src="/img/partner-chalmers.png" /></a></li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="drop-shadow-container">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="price-plan-container">
                                        <table class="price-plan">
                                                <tr>
                                                        <th>
                                                                <h4>Free</h4>
                                                                <em>För privatpersonen</em><br />
                                                                <br />
                                                                <p>0kr/mån</p>
                                                        </th>
                                                        <th>
                                                                <h4>Basic</h4>
                                                                <em>För egenföretagaren</em><br />
                                                                <br />
                                                                <p>499kr/mån</p>
                                                        </th>
                                                        <th>
                                                                <h4>Standard</h4>
                                                                <em>För småföretaget</em><br />
                                                                <br />
                                                                <p>999kr/mån</p>
                                                        </th>
                                                        <th>
                                                                <h4>Advanced</h4>
                                                                <em>Top of the line</em><br />
                                                                <br />
                                                                <p>4999kr/mån</p>
                                                        </th>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td><a href="#">1 användare</a></td>
                                                        <td><a href="#">1 användare</a></td>
                                                        <td><a href="#">2-9 användare</a></td>
                                                        <td><a href="#">10+ användare</a></td>
                                                </tr>
                                                <tr>
                                                        <td><a href="#">0 fria e-signaturer/mån</a></td>
                                                        <td><a href="#">10 fria e-signaturer/mån</a></td>
                                                        <td><a href="#">20 fria e-signaturer/mån</a></td>
                                                        <td><a href="#">200 fria e-signaturer/mån</a></td>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td><a href="#">100 dok/år fri lagring</a></td>
                                                        <td><a href="#">500 dok/år fri lagring</a></td>
                                                        <td><a href="#">1000 dok/år fri lagring</a></td>
                                                        <td><a href="#">5000 dok/år fri lagring</a></td>
                                                </tr>
                                                <tr class="big-row">
                                                        <td>
                                                                <p>Basfunktionalitet</p>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                        <td>
                                                                <p>Basfunktionalitet</p>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                        <td>
                                                                <p>Extra funktionalitet</p>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                        <td>
                                                                <p>Avancerad funktionalitet</p>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                </tr>
                                        </table>
                                        <div class="highlight-container">
                                                <div class="ribbon"></div>
                                                <ul class="highlight-rows">
                                                        <li class="top">
                                                                <h4>Standard</h4>
                                                                <em>För småföretaget</em><br />
                                                                <p>999kr/mån</p>
                                                        </li>
                                                        <li class="odd"><a href="#">2-9 användare</a></li>
                                                        <li>
                                                                <p><a href="#">20 fria e-signaturer/mån</a></p>
                                                        </li>
                                                        <li class="odd"><a href="#">1000 dok/mån</a></li>
                                                        <li class="bottom">
                                                                <p>Extra funktionalitet</p>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="drop-shadow-container">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="spacer70"></div>
                                <div class="try-skrivapa">
                                        <div class="left">
                                                <h2>2 månaders fri användning nu!</h2>
                                                <p>Allt inkluderat. Standardkonto, fria transaktioner och fri lagring.</p>
                                        </div>
                                        <div class="right">
                                                <form method="post" action="$linkrequestaccount$" class="requestAccount">
                                                        <input class="shiny infotext" name="email" type="email" value="Fyll i din e-postadress" /> <a href="#" class="submit btn-small green2 center float-right"><p>Skicka förfrågan!</p></a>
                                                        <input type="submit" class="hidden">
                                                </form>
                                        </div>
                                </div>
                                <div class="clearfix"></div>
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
                                                <!-- <li><a href="benefits.html">Fördelar</a></li> -->
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
                </div>
        </body>
</html>
####
