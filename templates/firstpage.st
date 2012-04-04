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
               <script src="$prefix$/js/validation.js"
                type="text/javascript"></script>
                <script src="/js/jquery.tweet.js" type="text/javascript"></script>
                $if(production)$
                <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
                $endif$
        </head>
        <body>
                $flashmessages()$
                $loginModal()$
                $legalModal()$
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
                                        <a href="$linkupload$" class="enter-button"><div>$_enter()$</div></a>
                                        $else$
                                        <a href="#" class="enter-button login-button"><div>$_login()$</div></a>
                                  $endif$
                                  <a href="$linkcontactus$"
                                  class="toplink ">$_pageHeaderContactUs()$</a>
                                  <a href="$linkabout$"
                                  class="toplink ">$_pageFooterAbout()$</a>
                                  <a href="$linklegal$" class="toplink ">$_pageFooterLaw()$</a>
                                  <a href="$linkpriceplan$"
                                  class="toplink ">$_pageFooterPrices()$</a>
                                  $localeSwitcher()$
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
                          <img class="first" src="/img/logo/pc-$if(langen)$en$else$se$endif$.png" />
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
                                                <tr class="big-row">
                                                        $_PriceTablePriceRow()$
                                                </tr>
                                        </table>
                                        <div class="highlight-container">
                                                <div class="ribbon $if(langen)$english$else$swedish$endif$"></div>
                                                $_PriceTableHighlightedColumn()$
                                        </div>
                                </div>
                                <div class="price-plan-annotation">
                                $_priceplanPageSignLimit()$
                                </div>

                <div class="big-price-plan">$_priceplanPageBelowTable1()$
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
###
legalModal=
<div class="modal-container overlay toscontainer" id="legalmodal">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h2>$_legalModalTitle()$</h2>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">

      <div>
        <img src="/img/Lawyer picture.jpg" class="float-left" />

        <p class="introtext">
          <b>Advokat, jur. dr Jon Kihlmans</b> krönika <i>Supply Chain Effect
          nr 1/2011</i> behandlar den övertro på olika varianter av
          skriftlig form som finns i näringslivet och bland många
          jurister. Artikeln kan även läsas
          som <a href="/supplychaineffectarticle.pdf">pdf</a>.
        </p>
      </div>

      <div class="clearfix"></div>
      <div class="spacer40"></div>

      <h3>Finns det papper och penna i molnet?</h3>

      <p>
        För en tid sedan träffade jag för en klients räkning ett
        förlikningsavtal i en tvist. Det var ingen stor fråga och det
        var inte fråga om särskilt mycket pengar. Jag föreslog för
        motpartsombudet att vi skulle dokumentera förlikningsavtalet
        genom att jag skrev ned vårt avtal, skickade det till henne
        med mejl och hon – om jag hade fått med allt – skrev OK och
        mejlade tillbaka. Motpartsombudet tyckte emellertid inte att
        det dög, utan ville absolut att vi skulle skriva på papper och
        att våra huvudmän själva skulle skriva under. Och avtal är ju
        som att dansa tango: It takes two! Så det fick bli papper och
        penna. Jag skrev ut avtalet och skickade det till min huvudman
        för underskrift. Så småningom kom det tillbaka – med vanlig
        post – och jag skickade det till motpartsombudet – med vanlig
        post – som skickade det till sin huvudman – med vanlig
        post. Så småningom fick jag en avi om att det fanns ett REK
        att hämta på posten. Jag gick dit och hämtade ut det slutligt
        undertecknade avtalet.
      </p>

      <h4>Skall det verkligen behöva vara så komplicerat?</h4>
      <p>
        Svaret på frågan är naturligtvis nej! Med några få undantag
        finns det inga formkrav för avtals giltighet, åtminstone inte
        i svensk rätt. Det finns – naturligtvis – inte heller några
        krav på att huvudmännen själva skall skriva under – eller vad
        de nu skall göra – avtalen. Det går alldeles utmärkt att lämna
        fullmakt. Och det kan – naturligtvis – ske formlöst: ”Kan inte
        du vara snäll och skriva under åt mig?”, duger alldeles
        utmärkt.
      </p>
      <p>
        Eftersom lagen för de flesta avtal inte kräver någon särskild
        form, blir också svaret på frågan om e-post är ett giltigt
        medel enkel: Självklart är e-post giltigt. Detsamma gäller med
        samma självklarhet andra former av elektronisk kommunikation.
      </p>
      <p>
        Men giltighet är inte det enda problemet att ta ställning
        till. Man måste också beakta behovet av att säkra bevisning –
        i det här fallet om att avtal har ingåtts. Duger e-post som
        bevisning? Vi har i Sverige vad som brukar kallas fri
        bevisprövning. Det innebär att all bevisning i och för sig är
        tillåten. Det är svårt att slå ett undertecknat dokument när
        fråga är om att bevisa förekomsten av ett avtal, men e-post är
        i de flesta fall mer än tillräcklig bevisning. Detsamma gäller
        andra former av elektronisk kommunikation.
      </p>
      <p>
        Många människor – inklusive en del motpartsombud – har en
        överdriven, närmast vidskeplig, tro på det nödvändiga i att
        avtal och andra rättshandlingar måste dokumenteras i
        pappersform och skrivas under med penna. En sådan inställning
        är ett hinder mot en effektivare affärskultur med fler och
        bättre avtal. För att inte tala om alla träd som måste sågas
        ner i onödan.
      </p>
      <p>
        Det finns inga juridiska hinder mot att i stort sett alla
        avtal ingås och dokumenteras i elektronisk form, antingen via
        e-post eller SMS eller genom att använda de lösningar för
        slutande av avtal med tillhörande arkivering som en del
        företag har börjat tillhandahålla. I molnet behövs varken
        papper eller penna. Jag ser fram emot att i framtiden slippa
        gå till posten för att hämta ut onödiga REK!
      </p>
    </div>
  </div>
  <div class="modal-footer">
    <a href="#" class="btn-small green float-right close">
      <div class="left"></div>
      <div class="label">
        <p>$_close()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
</div>
####
