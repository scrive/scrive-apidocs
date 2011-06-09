####
firstPage=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
        <head>
                <title>SkrivaPå | Signera och arkivera avtal elektroniskt</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                <meta http-equiv="content-language" content="sv" />
                <meta name="description" content="Signera och arkivera dina avtal elektroniskt. Spara tid, pengar och frustration – både åt dig och dina kunder." />
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
                $if(production)$
                <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
                $endif$
        </head>
        <body>
                $flashmessages()$
                $loginModal()$
                <div id="wrap">
                <div id="top-container">
                        <div class="content">
                                <img src="/img/logo.png" alt="SkrivaPå" class="logo" />
                                <div class="leftMain">
                                        <div class="intro-container">
                                                <h1>Varför krångla? - Signera med SkrivaPå!</h1>
                                                <p class="introtext">Signera avtal och andra dokument elektroniskt med SkrivaPå. <br />
                                                Våra kunder och våra kunders kunder älskar enkelheten.
                                                </p>
                                                <h3>Skapa konto nu!</h3>
                                                <p class="introtext">1 månad full funktionalitet utan kostnad. Inga förpliktelser.</p>
                                                <form method="post" action="$linksignup$" class="requestAccount">
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
                                        <a href="#"><h3>Skicka</h3>
                                        <p>Ladda upp dokument,<br />välj mottagare, skicka!</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col centre campaign-play" rel=".campaign-play-video">
                                        <div class="icon two"></div>
                                        <a href="#"><h3>Skriva på</h3>
                                        <p>Alla parter skriver på<br />elektroniskt.</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col right campaign-play" rel=".campaign-play-video">
                                        <div class="icon three"></div>
                                        <a href="#"><h3>Spara och arkivera</h3>
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
                                                <h2>Minska kostnaderna</h2>
                                                <p>Dokumenthantering är tidskrävande. Tid är pengar. SkrivaPå är ett skräddarsytt redskap som optimerar dina processer. Spara 50-250kr/dokument. </p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon watch"></div>
                                                </div>
                                                <h2>Snabbare avslut</h2>
                                                <p>Snabbare avslut leder till fler avslut. Vi hjälper dig att öka din omsättning.</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon globe"></div>
                                                </div>
                                                <h2>Överallt. Alltid.</h2>
                                                <p>Arbeta när du vill och var du vill. Glöm problemen med att buda dokument jorden runt.</p>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon seal"></div>
                                                </div>
                                                <h2>Starkare bevis</h2>
                                                <p>SkrivaPå verifierar transaktionen och stärker din bevisposition vid konflikt.</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon safe"></div>
                                                </div>
                                                <h2>100% kontroll</h2>
                                                <p>Vår säkra lagring garanterar dokumentens integritet. Äntligen full kontroll. </p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon face"></div>
                                                </div>
                                                <h2>Minska frustrationen</h2>
                                                <p>Det är sällan roligt att vända papper, leta efter förlorade dokument och jaga signaturer. SkrivaPå lättar din och dina kunders administrativa börda.</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon paper"></div>
                                                </div>
                                                <h2>Spara på miljön</h2>
                                                <p>Visa att ni tar ansvar. Skippa pappersavtalandet och minska din miljöpåverkan.</p>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon ipad"></div>
                                                </div>
                                                <h2>Image branding</h2>
                                                <p>Visa era kunder att ni är i framkant och gillar att tänka i nya banor.</p>
                                        </div>
                                </div>
                                <div class="clearfix"></div>
                                <div class="slide">
                                    <div class="slide-container">
                                        <h2>Kunder berättar</h2>
                                        <div class="slide-content active">
                							<div class="portrait-image"><img src="/img/logo-accountfactory.jpg"/></div>
                                            <blockquote>”Skönt. Snabbare vändor med kund och man slipper arkivera papper.”</blockquote>
                                            <em>Mikael Green, VD, Account Factory, Redovisningsbyrå</em>
                                        </div>
                                    <div class="slide-content">
                                        <div class="portrait-image"><img src="/img/logo-greenhatpeople.jpg"/></div>
                                        <blockquote>”Man har mycket bättre koll på de avtal som ingåtts. Det är grejen tycker jag.”</blockquote>
                                        <em>Alexis Barnekow, Försäljningschef, Green Hat People, Teambuilding Events</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"><img src="/img/logo-eventbook.jpg"/></div>
                                        <blockquote>”Smart lösning.”</blockquote>
                                        <em>Christoffer Lindquist, VD, Eventbook, Eventpartner</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"><img src="/img/logo-accenture.jpg"/></div>
                                        <blockquote>”Tjänsten är väldigt smidig och det visar verkligen hur mycket man kan effektivisera avtalsprocessen.”</blockquote>
                                        <em>Ola Brundin, Accenture Management Consulting</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"><img src="/img/logo-coop.jpg"/></div>
                                        <blockquote>”Snabbt och överskådligt.”</blockquote>
                                        <em>Michael Burman, Marknadschef, Coop Nord</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"></div>
                                        <blockquote>”Notera: efter 1 veckas användning börjar era kunder visa produkten för andra.”</blockquote>
                                        <em>Martin Lindström, IPAS</em>
                                    </div>
                                </div>
                                        <div class="slide-nav">
                                                <ul>
                                                        <li class="active"></li>
                                                        <li></li>
                                                        <li></li>
                                                        <li></li>
                                                        <li></li>
                                                        <li></li>
                                                </ul>
                                        </div>
                                        <div class="bubble">
                                                <h2>Partners</h2>
                                                <ul>
                                                        <li><a href="/partners"><img src="/img/partner-trustweaver.png"  /></a></li>
                                                        <li><a href="/partners"><img src="/img/partner-logica.png" /></a></li>
                                                        <li><a href="/partners"><img src="/img/partner-chalmers.png" /></a></li>
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
                                                                <h3>Small</h3>
                                                                <p>För sällananvändare</p>
                                                        </th>
                                                        <th>
                                                                <h3>Medium</h3>
                                                                <p>Låg volym och enkla behov</p>
                                                        </th>
                                                        <th>
                                                                <h3>Business</h3>
                                                                <p>Medel volym och många användningsområden</p>
                                                        </th>
                                                        <th>
                                                                <h3>Enterprise</h3>
                                                                <p>Stora volymer och/eller <BR/>komplexa behov</p>
                                                        </th>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td>1 användare</td>
                                                        <td>5 användare</td>
                                                        <td>15 användare</td>
                                                        <td>Obegränsat antal användare</td>
                                                </tr>
                                                <tr>
                                                        <td>10 fria signaturer/mån</td>
                                                        <td>20 fria signaturer/mån</td>
                                                        <td>50 fria signaturer/mån</td>
                                                        <td>Min 2.000 signaturer/år</td>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td>50 dok lagring</td>
                                                        <td>500 dok lagring</td>
                                                        <td>Obegränsad lagring</td>
                                                        <td>Obegränsad lagring</td>
                                                </tr>
                                                <tr>
                                                        <td>Individuellt arkiv</td>
                                                        <td>Dela arkiv</td>
                                                        <td>Dela arkiv</td>
                                                        <td>Dela arkiv</td>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td>Basfunktioner</td>
                                                        <td>Advancerade funktioner</td>
                                                        <td>Advancerade funktioner</td>
                                                        <td>Advancerade funktioner +</td>
                                                </tr>
                                                <tr class="big-row">
                                                        <td>
                                                                <h4>249kr/mån</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>
                                                        </td>
                                                        <td>
                                                                <h4>499kr/mån</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>
                                                        </td>
                                                        <td>
                                                                <h4>999kr/mån</h4>
                                                                <a href="/priser">Detaljerad prisplan<a>                                                   
                                                        </td>
                                                        <td>
                                                                <h4>Offert</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>
                                                        </td>
                                                </tr>
                                        </table>
                                        <div class="highlight-container">
                                                <div class="ribbon"></div>
                                                <ul class="highlight-rows">
                                                        <li class="top">
                                                                <h3>Business</h3>
                                                                <p>Medel volym och många användningsområden</p>
                                                        </li>
                                                        <li class="odd">15 användare</li>
                                                        <li>
                                                                <p>50 fria signaturer/mån</p>
                                                        </li>
                                                        <li class="odd">Obegränsad lagring</li>
                                                        <li>Dela arkiv</li>
                                                        <li class="odd">Avancerade funktioner</li>
                                                        <li class="bottom">
                                                                <h4>999kr/mån</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>   
                                                        </li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="drop-shadow-container">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="price-plan-annotation">* Alla priser är exkl moms<br/></div>
                                <div class="price-plan-annotation">Vi erbjuder också ett frikonto med 3 signaturer per månad och 10 dokument i lagring</div>
                                <div class="spacer70"></div>
                                <div class="try-skrivapa">
                                        <div class="left">
                                                <h3>Skapa konto nu!</h3>
                                                <p>1 månad full funktionalitet utan kostnad. Inga förpliktelser.</p>
                                        </div>
                                        <div class="right">
                                                <form method="post" action="$linksignup$" class="requestAccount">
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
                $pageFooter()$
            <div class="modal-container campaign-play-video" style="display:none;width:653px;height: 475px">
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
<iframe src="http://player.vimeo.com/video/22397410?title=0&amp;byline=0&amp;portrait=0&amp;color=ffffff" width="653" height="400" frameborder="0"></iframe>

                                            </div>
                                            </div>
           </div>
        </body>
</html>
####
