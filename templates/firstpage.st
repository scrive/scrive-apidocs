####
firstPage=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
        <head>
                <title>$_title()$</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                <meta http-equiv="content-language" content="sv" />
                <meta name="description" content="Signera och arkivera dina avtal elektroniskt. Spara tid, pengar och frustration – både åt dig och dina kunder." />
                <link href='$protocol$//fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css"/>
                <link href="/css/main.css" rel="stylesheet" type="text/css" />
                <link href="/theme/flash.css" rel="stylesheet" type="text/css" />
                <link rel="stylesheet" type="text/css" href="$prefix$/theme/symbols.css" media="screen"/>
                <link rel="stylesheet" type="text/css" href="$prefix$/theme/newdesign.css" media="screen"/>
                <link rel="stylesheet" type="text/css" href="$prefix$/theme/buttons.css" media="screen"/>
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
               <script type="text/javascript">
                $javascriptLocalisation()$</script>
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
                                                <h1>$_h1()$</h1>
                                                <p class="introtext">$_h1Body()$ 
                                                </p>
                                                <h3>$_h3CreateAccount()$</h3>
                                                <p class="introtext">$_h3CreateAccountBody()$</p>
                                                <form method="post" action="$linksignup$" class="requestAccount">
                                                        <input class="shiny" type="email" name="email" infotext="Fyll i din e-postadress" /> <a href="#" class="submit new-btn-small green2 center"><p>Skapa konto!</p></a>
                                                        <input type="submit" class="hidden" />
                                                </form>
                                        </div>
                                </div>
                                <div class="rightMain">
                                        <a href="#" class="login-button"><p>$_login()$</p></a>
                                        <div class="campaign-container">
                                                <div class="campaign-play" rel=".campaign-play-video"></div>
                                                <div class="campaign-text" rel=".campaign-play-video">
                                                        <blockquote>$_QuoteOnVid()$</blockquote>
                                                        <em>$_QuoteOnVidBy()$</em>
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
                                        <a href="#"><h3>$_StepsH3Send()$</h3>
                                        <p>$_StepsH3SendBody()$</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col centre campaign-play" rel=".campaign-play-video">
                                        <div class="icon two"></div>
                                        <a href="#"><h3>$_StepsH3Sign()$</h3>
                                        <p>$_StepsH3SignBody()$</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col right campaign-play" rel=".campaign-play-video">
                                        <div class="icon three"></div>
                                        <a href="#"><h3>$_StepsH3Store()$</h3>
                                        <p>$_StepsH3StoreBody()$</p></a>
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
                                                <h2>$_Bemefit1()$</h2>
                                                <p>$_Bemefit1Body()$</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon watch"></div>
                                                </div>
                                                <h2>$_Bemefit2()$</h2>
                                                <p>$_Bemefit2Body()$</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon globe"></div>
                                                </div>
                                                <h2>$_Bemefit3()$</h2>
                                                <p>$_Bemefit3Body()$</p>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon seal"></div>
                                                </div>
                                                <h2>$_Bemefit4()$</h2>
                                                <p>$_Bemefit4Body()$</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon safe"></div>
                                                </div>
                                                <h2>$_Bemefit5()$</h2>
                                                <p>$_Bemefit5Body()$</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon face"></div>
                                                </div>
                                                <h2>$_Bemefit6()$</h2>
                                                <p>$_Bemefit6Body()$</p>
                                        </div>
                                        <div class="col">
                                                <div class="icon-container">
                                                        <div class="icon paper"></div>
                                                </div>
                                                <h2>$_Bemefit7()$</h2>
                                                <p>$_Bemefit7Body()$</p>
                                        </div>
                                        <div class="col no-margin">
                                                <div class="icon-container">
                                                        <div class="icon ipad"></div>
                                                </div>
                                                <h2>$_Bemefit8()$</h2>
                                                <p>$_Bemefit8Body()$</p>
                                        </div>
                                </div>
                                <div class="clearfix"></div>
                                <div class="slide">
                                    <div class="slide-container">
                                        <h2>$_QuotesHead()$</h2>
                                        <div class="slide-content active">
                							<div class="portrait-image"><img src="/img/logo-accountfactory.jpg"/></div>
                                            <blockquote>$_Quote1()$</blockquote>
                                            <em>$_Quote1By()$</em>
                                        </div>
                                    <div class="slide-content">
                                        <div class="portrait-image"><img src="/img/logo-greenhatpeople.jpg"/></div>
                                        <blockquote>$_Quote2()$</blockquote>
                                        <em>$_Quote2By()$</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"><img src="/img/logo-eventbook.jpg"/></div>
                                        <blockquote>$_Quote3()$</blockquote>
                                        <em>$_Quote3By()$</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"><img src="/img/logo-accenture.jpg"/></div>
                                        <blockquote>$_Quote4()$</blockquote>
                                        <em>$_Quote4By()$</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"><img src="/img/logo-coop.jpg"/></div>
                                        <blockquote>$_Quote5()$</blockquote>
                                        <em>$_Quote5By()$</em>
                                    </div>
                                    <div class="slide-content">
								        <div class="portrait-image"></div>
                                        <blockquote>$_Quote6()$</blockquote>
                                        <em>$_Quote6By()$</em>
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
                                                <h2>$_PartnersHead()$</h2>
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
                                                                <p>Förbättra processerna i den mindre organisationen</p>
                                                        </th>
                                                        <th>
                                                                <h3>Team</h3>
                                                                <p>Effektivisera teamet med enkla medel</p>
                                                        </th>
                                                        <th>
                                                                <h3>Business</h3>
                                                                <p>En helhetslösning för många användningsområden</p>
                                                        </th>
                                                        <th>
                                                                <h3>Enterprise</h3>
                                                                <p>Större volymer, integrationer 
och anpassning</p>
                                                        </th>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td>5 användare</td>
                                                        <td>15 användare</td>
                                                        <td>50 användare</td>
                                                        <td>Offert</td>
                                                </tr>
                                                <tr>
                                                        <td>500 signaturer/år</td>
                                                        <td>1000 signaturer/år</td>
                                                        <td>2000 signaturer/år</td>
                                                        <td>Från 4000 signaturer/år</td>
                                                </tr>
                                                <tr class="odd-row">
                                                        <td>500 dok laging</td>
                                                        <td>Obegränsad lagring</td>
                                                        <td>Obegränsad lagring</td>
                                                        <td>Offert</td>
                                                </tr>
                                                <tr>
                                                        <td>Basfunktioner</td>
                                                        <td>Basfunktioner</td>
                                                        <td>Advancerade funktioner</td>
                                                        <td>Anpassad lösning</td>
                                                </tr>
                                                <tr class="big-row">
                                                        <td>
                                                                <h4>35 kr/signatur</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>
                                                        </td>
                                                        <td>
                                                                <h4>32.50 kr/signatur</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>
                                                        </td>
                                                        <td>
                                                                <h4>30 kr/signatur</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>                                                   
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
                                                                <p>En helhetslösning för många användningsområden </p>
                                                        </li>
                                                        <li class="odd">50 användare</li>
                                                        <li>
                                                                <p>2000 signaturer/år</p>
                                                        </li>
                                                        <li class="odd">Obegränsad lagring</li>
                                                        <li>Avancerade funktioner</li>
                                                        <li class="bottom">
                                                                <h4>30 kr/signatur</h4>
                                                                <a href="/priser">Detaljerad prisplan</a>   
                                                        </li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="drop-shadow-container">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="price-plan-annotation">
                                * Alla priser är exkl moms.<br/>
                                </div>
                                <div class="price-plan-details">
                                  <h3>Gratis för privatpersoner och enmansföretagare</h3>
                                  <p>Det finns tusen skäl varför alla bör signera sina dokument elektroniskt. Vår ambition är att alltid jobba nära kunden i tätt samarbete. Tyvärr har vi inte möhlighet att hjälpa alla målgrupper aktivt. Därför erbjuder vi ett supportfritt konto för alla mindre användare. 1 användare, 5 fria signaturer per månad och 50 dokument i lagring.</p>
                                </div>
                                <div class="spacer70"></div>
                                <div class="try-skrivapa">
                                        <div class="left">
                                                <h3>$_h3CreateAccount()$</h3>
                                                <p>$_h3CreateAccountBody()$</p>
                                        </div>
                                        <div class="right">
                                                <form method="post" action="$linksignup$" class="requestAccount">
                                                        <a href="#" class="submit new-btn-small green2 center float-right"><p>$_ButtonCreateAccount()$</p></a>
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
                                                        <h3>$_DemovidHead()$</h3>
                                                        <p>$_DemovidBody()$</p>
                                                    </span>
                                                <a class="modal-close close"> </a>
                                            </div>
                                            <div class="modal-body" style="padding:0px">
                                            <div class="modal-content">
<iframe src="http://player.vimeo.com/video/22397410?title=0&amp;byline=0&amp;portrait=0&amp;color=ffffff" width="653" height="400" frameborder="0"></iframe>

                                            </div>
                                            </div>
           </div>
         </div>
        </body>
</html>
####
