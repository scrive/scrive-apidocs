####
firstPage=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
        <head>
                <title>SkrivaPå</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                $facebooklogo()$
                <link href='$protocol$//fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css">
                <link href="/css/main.css" rel="stylesheet" type="text/css" />
                <link href="/theme/flash.css" rel="stylesheet" type="text/css" />
                <link rel="stylesheet" type="text/css" href="$prefix$/theme/symbols.css" media="screen">
                <link rel="stylesheet" type="text/css" href="$prefix$/theme/newdesign.css" media="screen">
                <link rel="stylesheet" type="text/css" href="$prefix$/theme/buttons.css" media="screen">
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
                <script src="/js/jquery.tweet.js" type="text/javascript"></script>
                <script src="/js/main.js" type="text/javascript"></script>
                <script type="text/javascript">
                        // <![CDATA[
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
                                jQuery('.campaign-text a, .campaign-play').overlay({mask: standardDialogMask});
                                
                                jQuery('.slide-nav ul li').click( function() {
                                        var index = jQuery(this).index();
                                        
                                        gotoPage('.slide-content', index);
                                });
                                
                                doSlide('.slide-content');
                        });
                // ]]>
                </script>
        </head>
        <body>
                $flashmessages()$
                $loginModal()$
                <div id="wrap">
                <div id="top-container">
                        <div class="content">
                                <div class="logo no-txt">SkrivaPå</div>
                                <div class="leftMain">
                                        <div class="intro-container">
                                                <h2>Varför krångla?</h2>
                                                <h3>SkrivaPå är en tjänst för att skriva på och arkivera avtal elektroniskt. <BR/> Våra kunder och våra kunders kunder älskar enkelheten.
                                                <br />
                                                <br />
                                                <br />
                                                </h3>
                                                <h2>Skapa konto nu!</h2>
                                                <h3>1 månad full funktionalitet utan kostnad. Inga förpliktelser.</h3>
                                                <form method="post" action="/signup" class="requestAccount">
                                                        <input class="shiny" type="email" name="email" infotext="Fyll i din e-postadress" /> <a href="#" class="submit new-btn-small green2 center"><p>Skapa konto!</p></a>
                                                        <input type="submit" class="hidden" />
                                                </form>
                                        </div>
                                </div>
                                <div class="rightMain">
                                        <a href="#" class="login-button"><p>Logga in</p></a>
                                        <div class="campaign-container">
                                                <div class="campaign-play" rel=".campaign-play-video"></div>
                                                <div class="campaign-text" rel=".campaign-play-video">
                                                        <blockquote>”Processerna blir betydligt smidigare.”</blockquote>
                                                        <em>- Lisa Renander och Julia Hamrin, VD och Säljchef på Go Enterprise</em>
                                                </div>
                                        </div>
                                </div>
                                <div class="person"></div>
                        </div>
                </div>
                <div id="nav-container">
                        <div class="content">
                                <div class="col left campaign-play" rel=".campaign-play-video">
                                        <div class="icon one"></div>
                                        <a href="#"><h4>Skicka</h4>
                                        <p>Ladda upp dokument,<br />välj mottagare, skicka!</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col centre campaign-play" rel=".campaign-play-video">
                                        <div class="icon two"></div>
                                        <a href="#"><h4>Skriva på</h4>
                                        <p>Alla parter skriver på<br />elektroniskt.</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col right campaign-play" rel=".campaign-play-video">
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
                                                <h5>Minska kostnaderna</h5>
                                                <p>Dokumenthantering är tidskrävande. Tid är pengar. SkrivaPå är ett skräddarsytt redskap som optimerar dina processer. Spara 50-250kr/dokument. </p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon watch"></div>
                                                </div>
                                                <h5>Snabbare avslut</h5>
                                                <p>Snabbare avslut leder till fler avslut. Vi hjälper dig att öka din omsättning.</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon globe"></div>
                                                </div>
                                                <h5>Överallt. Alltid.</h5>
                                                <p>Arbeta när du vill och var du vill. Glöm problemen med att buda dokument jorden runt.</p>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon seal"></div>
                                                </div>
                                                <h5>Starkare bevis</h5>
                                                <p>SkrivaPå verifierar transaktionen och stärker din bevisposition vid konflikt.</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon safe"></div>
                                                </div>
                                                <h5>100% kontroll</h5>
                                                <p>Vår säkra lagring garanterar dokumentens integritet. Äntligen full kontroll. </p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon face"></div>
                                                </div>
                                                <h5>Minska frustrationen</h5>
                                                <p>Det är sällan roligt att vända papper, leta efter förlorade dokument och jaga signaturer. SkrivaPå lättar din och dina kunders administrativa börda.</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon paper"></div>
                                                </div>
                                                <h5>Spara på miljön</h5>
                                                <p>Visa att ni tar ansvar. Skippa pappersavtalandet och minska din miljöpåverkan.</p>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon ipad"></div>
                                                </div>
                                                <h5>Image branding</h5>
                                                <p>Visa era kunder att ni är i framkant och gillar att tänka i nya banor.</p>
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
                                                                <p>För sällananvändare</p>
                                                        </th>
                                                        <th>
                                                                <h4>Small</h4>
                                                                <p>Låg volym och enkla behov</p>
                                                        </th>
                                                        <th>
                                                                <h4>Business</h4>
                                                                <p>Medel volym och många användningsområden</p>
                                                        </th>
                                                        <th>
                                                                <h4>Enterprise</h4>
                                                                <p>Stora volymer och/eller <BR/>komplexa behov</p>
                                                        </th>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td><a href="#">1 användare</a></td>
                                                        <td><a href="#">1 användare</a></td>
                                                        <td><a href="#">2-9 användare</a></td>
                                                        <td><a href="#">Obegränsat antal användare</a></td>
                                                </tr>
                                                <tr>
                                                        <td><a href="/priceplan.html?nocolumns">3 fria signaturer/mån</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">15 fria signaturer/mån</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">25 fria signaturer/mån</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Min 1000 signaturer/år</a></td>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td><a href="/priceplan.html?nocolumns">50 dok lagring</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">500 dok lagring</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Obegränsad lagring</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Obegränsad lagring</a></td>
                                                </tr>
                                                <tr>
                                                        <td><a href="/priceplan.html?nocolumns">Individuellt arkiv</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Individuellt arkiv</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Dela arkiv</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Dela arkiv</a></td>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td><a href="/priceplan.html?nocolumns">Basfunktioner</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Basfunktioner +</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Avancerade funktioner</a></td>
                                                        <td><a href="/priceplan.html?nocolumns">Avancerade funktioner +</a></td>
                                                </tr>
                                                <tr class="big-row">
                                                        <td>
                                                                <h4>0kr</h4>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                        <td>
                                                                <h4>299kr</h4>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                        <td>
                                                                <h4>499kr</h4>
                                                                <a href="priceplan.html?nocolumns">Läs mer<a>                                                   
                                                        </td>
                                                        <td>
                                                                <h4>Offert</h4>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>
                                                        </td>
                                                </tr>
                                        </table>
                                        <div class="highlight-container">
                                                <div class="ribbon"></div>
                                                <ul class="highlight-rows">
                                                        <li class="top">
                                                                <h4>Business</h4>
                                                                <p>Medel volym och många användningsområden</p>
                                                        </li>
                                                        <li class="odd"><a href="#">2-9 användare</a></li>
                                                        <li>
                                                                <p><a href="/priceplan.html?nocolumns">25 fria signaturer/mån</a></p>
                                                        </li>
                                                        <li class="odd"><a href="/priceplan.html?nocolumns">Obegränsad lagring</a></li>
                                                        <li><a href="#">Dela arkiv</a></li>
                                                        <li class="odd"><a href="/priceplan.html?nocolumns">Avancerade funktioner</a></li>
                                                        <li class="bottom">
                                                                <h4>499kr</h4>
                                                                <a href="priceplan.html?nocolumns">Läs mer</a>   
                                                        </li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="drop-shadow-container">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="price-plan-annotation">* Alla priser är exkl moms</div>
                                <div class="spacer70"></div>
                                <div class="try-skrivapa">
                                        <div class="left">
                                                <h2>Skapa konto nu!</h2>
                                                <p>1 månad full funktionalitet utan kostnad. Inga förpliktelser.</p>
                                        </div>
                                        <div class="right">
                                                <form method="post" action="/signup" class="requestAccount">
                                                        <a href="#" class="submit new-btn-small green2 center float-right"><p>Skapa konto!</p></a>
                                                        <input class="shiny float-left" name="email" type="email" infotext="Fyll i din e-postadress" />
                                                        <input type="submit" class="hidden" />
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
                                                <!--<li><a href="features.html?columns">Funktioner</a></li>-->
                                                <!-- <li><a href="benefits.html">Fördelar</a></li> -->
                                                <li><a href="priceplan.html">Prisplan</a></li>
                                        </ul>
                                        <br />
                                        <strong>TRYGGHET OCH VILLKOR</strong>
                                        <ul>
                                                <li><a href="security.html?columns">Säkerhet</a></li>
                                                <li><a href="legal.html?columns">Juridik</a></li>
                                                <li><a href="privacypolicy.html?columns">Sekretesspolicy</a></li>
                                                <li><a href="terms.html?columns">SkrivaPå Allmäna Villkor</a></li>
                                        </ul>
                                        <br />
                                        <strong>SKRIVAPÅ</strong>
                                        <ul>
                                                <li><a href="about.html?columns">Om SkrivaPå</a></li>
                                                <li><a href="partners.html?columns">Partners</a></li>
                                                <li><a href="clients.html?columns">Kunder</a></li>
                                                <!--<li><a href="board.html?columns">Board of Advisors</a></li>-->
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
                                                        Saltmätargatan 19<br />
                                                        113 59 Stockholm
                                                </li>
                                        </ul>
                                </div>
                        </div>
                </div>
                </div>
            <div class="modal-container campaign-play-video" style="display:none;width:900px;height: 625px">
                                            <div class="modal-header">
                                                <span class="modal-icon message"></span>
                                                    <span class="modal-title">
                                                        <h3>Demonstration</h3>
                                                        <p>Följande är ett demonstrationsklipp av hur man undertecknar ett dokument med vår tjänst.</p>
                                                    </span>
                                                <a class="modal-close close"> </a>
                                            </div>
                                            <div class="modal-body" style="padding:0px">
                                            <div class="modal-content">
<iframe src="http://player.vimeo.com/video/22397410?title=0&amp;byline=0&amp;portrait=0&amp;color=ffffff" width="900" height="551" frameborder="0"></iframe>

                                            </div>
                                            </div>
           </div>
        </body>
</html>
####
