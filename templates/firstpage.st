####
firstPage=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
        <head>
                <title>$_title()$</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                <meta http-equiv="content-language" content="$ctxlang$" />
                <meta name="description" content="$_firstPageMetaDescription()$" />
                <link href='https://fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css"/>
                <script src="/localization/$ctxlang$/$versioncode$.js"  type="text/javascript"></script>
                $staticResources:noescape()$
<!-- start Mixpanel --><script type="text/javascript">(function(e,b){if(!b.__SV){var a,f,i,g;window.mixpanel=b;a=e.createElement("script");a.type="text/javascript";a.async=!0;a.src=("https:"===e.location.protocol?"https:":"http:")+'//cdn.mxpnl.com/libs/mixpanel-2.2.min.js';f=e.getElementsByTagName("script")[0];f.parentNode.insertBefore(a,f);b._i=[];b.init=function(a,e,d){function f(b,h){var a=h.split(".");2==a.length&&(b=b[a[0]],h=a[1]);b[h]=function(){b.push([h].concat(Array.prototype.slice.call(arguments,0)))}}var c=b;"undefined"!==
typeof d?c=b[d]=[]:d="mixpanel";c.people=c.people||[];c.toString=function(b){var a="mixpanel";"mixpanel"!==d&&(a+="."+d);b||(a+=" (stub)");return a};c.people.toString=function(){return c.toString(1)+".people (stub)"};i="disable track track_pageview track_links track_forms register register_once alias unregister identify name_tag set_config people.set people.increment".split(" ");for(g=0;g<i.length;g++)f(c,i[g]);b._i.push([a,e,d])};b.__SV=1.2}})(document,window.mixpanel||[]);
mixpanel.init("5b04329b972851feac0e9b853738e742");</script><!-- end Mixpanel -->
        </head>
        <body>
                $flashmessages()$
                $loginModal()$
                <div id="wrap">
                <div id="top-container">
                        <div class="content">
                                <img src="/img/logo-scrive.png" alt="Scrive" class="logo" />
                                <div class="leftMain">

                                        <div class="intro-container">
                                                <h1>$_h1()$</h1>
                                                <p class="introtext">$_h1Body()$</p>
                                                <h3>$_legalTitleFrontPage()$</h3>
                                                <p class="introtext">$_legalBodyFrontPage()$</p>
                                                <h3>$_h3CreateAccount()$</h3>
                                                <p class="introtext">$_h3CreateAccountBody()$</p>
                                                <form method="post" action="$linksignup$" class="requestAccount">
                                                  <input class="shiny" type="email" name="email" infotext="$_FillInEmail()$" autocomplete="off" />
                                                  <a href="#" class="submit new-btn-small green2 center"><p>$_ButtonCreateAccount()$</p></a>
                                                  <input type="submit" class="hidden" />
                                                </form>
                                        </div>
                                </div>
                                <div class="rightMain">
                                  <span class="moveleft">
                                  $if(logged)$
                                        <a href="/newdocument" class="enter-button"><div>$_enter()$</div></a>
                                        $else$
                                        <a href="#" class="enter-button login-button"><div>$_login()$</div></a>
                                  $endif$
                                  <a href="$linkcontactus$"
                                  class="toplink ">$_pageHeaderContactUs()$</a>
                                  <a href="$linkabout$"
                                  class="toplink ">$_pageFooterAbout()$</a>
                                  <a href="$linkclients$" class="toplink ">$_customerCases()$</a>
                                  <a href="$linkpriceplan$"
                                  class="toplink ">$_pageFooterPrices()$</a>
                                  $langSwitcher()$
                                  </span>

                                        <div class="campaign-container">
                                                <div class="campaign-play campaign-play-video"></div>
                                                <div class="campaign-text campaign-play-video">
                                                        <blockquote>$_QuoteOnVid()$</blockquote>
                                                        <em>$_QuoteOnVidBy()$</em>
                                                </div>
                                        </div>
                                </div>
                                <div class="person"></div>
                        </div>
                </div>
                <div class="nav-container">
                        <div class="logos">
                          <img class="first" src="/img/logo/pc-$if(langenglish)$en$else$se$endif$.png" />
                        </div>
                        <div class="bottom-border"></div>
                </div>
                <div class="body-container">
                        <div class="content">
                                <div class="spacer40"></div>
                                <div class="benefitsbox">
                                <h2 class="benefitstitle">$_benefitsTitle()$</h2>
                                <div class="spacer20"></div>
                                <div>
                                        <div class="smallbox float-right">
                                                <div class="icon-container">
                                                        <div class="icon-money"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Bemefit1()$</h2>
                                                  <p>$_Bemefit1Body()$</p>
                                                </div>
                                        </div>
                                        <div class="smallbox float-left">
                                                <div class="icon-container">
                                                        <div class="icon-watch"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Bemefit2()$</h2>
                                                  <p>$_Bemefit2Body()$</p>
                                                </div>

                                        </div>
                                        <div class="smallbox float-right">
                                                <div class="icon-container">
                                                        <div class="icon-seal"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Bemefit4()$</h2>
                                                  <p>$_Bemefit4Body()$</p>
                                                </div>
                                        </div>
                                        <div class="smallbox float-left">
                                                <div class="icon-container">
                                                        <div class="icon-safe"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Bemefit5()$</h2>
                                                  <p>$_Bemefit5Body()$</p>
                                                </div>
                                        </div>
                                </div>
                                </div>

                                <div class="productdescriptionbox">
                                <div class="separator"> </div>
                                <h2 class="productdescriptiontitle">$_productDescriptionTitle()$</h2>
                                <div class="spacer20"></div>
                                <div>
                                       <div class="smallbox float-left">
                                                <div class="icon-container">
                                                        <div class="icon-question"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Product4()$</h2>
                                                  <p>$_Product4Body()$</p>
                                                </div>
                                        </div>
                                        <div class="smallbox float-right">
                                                <div class="icon-container">
                                                        <div class="icon-cloud"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Product1()$</h2>
                                                  <p>$_Product1Body()$</p>
                                                </div>
                                        </div>
                                        <div class="smallbox float-left">
                                                <div class="icon-container">
                                                        <div class="icon-mail"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Product2()$</h2>
                                                  <p>$_Product2Body()$</p>
                                                </div>
                                        </div>
                                        <div class="smallbox float-right">
                                                <div class="icon-container">
                                                        <div class="icon-api"></div>
                                                </div>
                                                <div class="text-container">
                                                  <h2>$_Product3()$</h2>
                                                  <p>$_Product3Body()$</p>
                                                </div>
                                        </div>

                                </div>
                                </div>
                                <div class="spacer50"></div>
                                <div class="clearfix"></div>
<!--
                                <div class="price-plan-container">
                                        <table class="price-plan">
                                                <tr>$_PriceTableHead()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow1()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow2()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow3()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow4()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow5()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow6()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow7()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow8()$
                                                </tr>
                                                <tr class="big-row last-child">
                                                        $_PriceTablePriceRow()$
                                                </tr>
                                        </table>
                                        <div class="highlight-container">
                                                <div class="ribbon $if(langenlish)$english$else$swedish$endif$"></div>
                                                $_PriceTableHighlightedColumn()$
                                        </div>
                                </div>
                                <div class="price-plan-annotation">
                                $_priceplanPageSignLimit()$
                                </div>

                <div class="big-price-plan">$_priceplanPageBelowTable1()$
                </div>

                                <div class="spacer70"></div>
-->
                                <div class="try-skrivapa">
                                        <div class="left">
                                                <h3>$_h3CreateAccount()$</h3>
                                                <p>$_h3CreateAccountBody()$</p>
                                        </div>
                                        <div class="right">
                                                <form method="post" action="$linksignup$" class="requestAccount">
                                                        <a href="#" class="submit new-btn-small green2 center float-right"><p>$_ButtonCreateAccount()$</p></a>
                                                        <input class="shiny float-left" name="email" type="email" infotext="$_FillInEmail()$"  autocomplete="off"/>
                                                        <input type="submit" class="hidden" />
                                                </form>
                                        </div>
                                </div>
                                <div class="clearfix"></div>
                        </div>
                        <div class="spacer40"></div>
                </div>
                $pageFooter()$
         </div>
         <script>
            $if(user.id)$mixpanel.identify("$user.id$");$endif$
            mixpanel.track('view homepage');
         </script>
        </body>
</html>
####
