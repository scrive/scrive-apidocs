####
firstPage=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
        <head>
                <title>$_title()$</title>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                <meta http-equiv="content-language" content="$ctxlang$" />
                <meta name="description" content="$_firstPageMetaDescription()$" />
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
               <script src="$prefix$/js/underscore.js"
                type="text/javascript"></script>
               <script src="$prefix$/js/backbone.js"
                type="text/javascript"></script>
               <script src="$prefix$/js/flashmessages.js"
                type="text/javascript"></script>
                <script src="/js/jquery.tweet.js" type="text/javascript"></script>
                <script src="/js/main.js" type="text/javascript"></script>
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
                                <img src="$ourFirstPageLogo()$" alt="$ourServiceName()$" class="logo" />
                                <div class="leftMain">
                                        
                                        $localeSwitcher()$
                                        <div class="intro-container">
                                                <h1>$_h1()$</h1>
                                                <p class="introtext">$_h1Body()$ 
                                                </p>
                                                <h3>$_h3CreateAccount()$</h3>
                                                <p class="introtext">$_h3CreateAccountBody()$</p>
                                                <form method="post" action="$linksignup$" class="requestAccount">
                                                        <input class="shiny"
                                                        type="email"
                                                        name="email"
                                                        infotext="$_FillInEmail()$"
                                                        autocomplete="off"
                                                        /> <a href="#" class="submit new-btn-small green2 center"><p>$_ButtonCreateAccount()$</p></a>
                                                        <input type="submit" class="hidden" />
                                                </form>
                                        </div>
                                </div>
                                <div class="rightMain">
                                  <span id="moveleft">
                                  <a href="$linkabout$"
                                  class="document-button">$_pageFooterAbout()$</a>
                                  <a href="$linklegal$" class="document-button">$_pageFooterLaw()$</a>
                                  <a href="$linkpriceplan$"
                                  class="document-button">$_pageFooterPrices()$</a>
                                  </span>
                                        $if(logged)$
                                        <a href="$linkupload$" class="enter-button"><p>$_enter()$</p></a>
                                        $else$
                                        <a href="#" class="enter-button login-button"><p>$_login()$</p></a>
                                        $endif$
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
                <div id="nav-container">
                        <div class="content">
                                <div class="col left campaign-play campaign-play-video">
                                        <div class="icon one"></div>
                                        <a href="#"><h3>$_StepsH3Send()$</h3>
                                        <p>$_StepsH3SendBody()$</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col centre campaign-play campaign-play-video">
                                        <div class="icon two"></div>
                                        <a href="#"><h3>$_StepsH3Sign()$</h3>
                                        <p>$_StepsH3SignBody()$</p></a>
                                </div>
                                <div class="divider-hor"></div>
                                <div class="col right campaign-play campaign-play-video">
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
                                <div class="slide" style="display:none">
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
                                                        <li><a href="$linkpartners$"><img src="/img/partner-trustweaver.png"  /></a></li>
                                                        <li><a href="$linkpartners$"><img src="/img/partner-logica.png" /></a></li>
                                                        <li><a href="$linkpartners$"><img src="/img/partner-chalmers.png" /></a></li>
                                                </ul>
                                        </div>
                                </div>
                                <div class="drop-shadow-container" style="display:none">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="price-plan-container">
                                        <table class="price-plan">
                                                <tr>$_PriceTableHead()$
                                                </tr>
                                                <tr class="odd-row">
                                                        $_PriceTableRow1()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow2()$
                                                </tr>
                                                <tr class="odd-row">
                                                        $_PriceTableRow3()$
                                                </tr>
                                                <tr>
                                                        $_PriceTableRow4()$
                                                </tr>
                                                <tr class="big-row">
                                                        $_PriceTablePriceRow()$
                                                </tr>
                                        </table>
                                        <div class="highlight-container">
                                                <div class="ribbon $if(langen)$english$else$swedish$endif$"></div>
                                                $_PriceTableHighlightedColumn()$
                                        </div>
                                </div>
                                <div class="drop-shadow-container">
                                        <div class="drop-shadow"></div>
                                </div>
                                <div class="price-plan-annotation">
                                $_priceplanPageVAT()$
                                </div>
                                <div class="price-plan-details">
                                  $_BelowPriceTable1()$
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
        $if(production)$
        <!-- Snoobi code tracking code -->
            <script type="text/javascript">
            var psSite = "8d72086524";
            var peJsHost = "https://";
            document.write(unescape("%3Cscript src='https://tr.prospecteye.com/track.js' type='text/javascript'%3E%3C/script%3E"));
            </script>
            <noscript><img src="https://tr.prospecteye.com/?id=8d72086524" style="border:0;display:none;"/></noscript>
       <!-- End of snoobi -->
       $endif$
        </body>
</html>
####
