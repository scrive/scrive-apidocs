###
notFound=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<div class='short-input-section'>
  <div class="shadowed">
    $_notFound()$
  </div>
  <div class='short-input-container'>
    <div class='short-input-container-body-wrapper'>
      <div class='short-input-container-body'>
        <p class='position first'>
         $_afterErrorGoToInstruction()$
        </p>
        <p class="position" style='height:44px;'>
          <a class='button float-left' style='width:100px;' href='/'>
            $_afterErrorGoHomepage()$
          </a>
          <a href='#' class='button float-right' style='width:100px;' onClick='history.go(-1);return false;'>
            $_afterErrorGoBack()$
          </a>
        </p>
      </div>
    </div>
  </div>
</div>
</div>
<script>
 mixpanel.track('Error', {
   Message : '404',
   URL : window.location.href
 });
</script>
###
internalServerError=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<div class='short-input-section'>
  <div class="shadowed">
   $_internalServerError()$
  </div>
  <div class='short-input-container'>
    <div class='short-input-container-body-wrapper'>
      <div class='short-input-container-body'>
        <p class='position first'>
         $_afterErrorGoToInstruction()$
        </p>
        <p class="position" style='height:44px;'>
          <a class='button float-left' style='width:100px;' href='/'>
            $_afterErrorGoHomepage()$
          </a>
          <a href='#' class='button float-right' style='width:100px;' onClick='history.go(-1);return false;'>
            $_afterErrorGoBack()$
          </a>
        </p>
      </div>
    </div>
  </div>
</div>
</div>
<script>
 mixpanel.track('Error', {
   Message : '500',
   URL : window.location.href
 });
</script>
###
wholePage=
    $doctype()$
    $htmlopentag()$
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
      <body $if(custombackground)$style="background:$custombackground$;"$endif$>
        <div class="wrapper-position-footer">
         $header()$
         $flashmessages()$
         $content:noescape()$
         $contactUsVivocha()$
         $pageFooter()$
 	</div>
      </body>
    $htmlclosingtag()$
####
pagetitle=$title$$if(production)$$else$ (devel) $endif$
####
doctype=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
####
htmlclosingtag=
</html>
####
htmlopentag=
    <!--[if lt IE 7]>  <html class="ie ie6 lte9 lte8 lte7"> <![endif]-->
    <!--[if IE 7]>     <html class="ie ie7 lte9 lte8 lte7"> <![endif]-->
    <!--[if IE 8]>     <html class="ie ie8 lte9 lte8"> <![endif]-->
    <!--[if IE 9]>     <html class="ie ie9 lte9"> <![endif]-->
    <!--[if gt IE 9]>  <html> <![endif]-->
    <!--[if !IE]> -->  <html><!-- <![endif]-->
####
standardHeaderContents=
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <meta http-equiv="content-language" content="$ctxlang$" />
        <meta http-equiv="X-UA-Compatible" content="IE=10" />
        <meta name="viewport" content="width=1020" />
        <!--[if lt IE 9]>
	<script src="/bower_components/es5-shim/es5-shim.js"></script>
	<script src="/bower_components/es5-shim/es5-sham.js"></script>
	<script src="/libs/html5shiv.js"></script>
	<script src="/bower_components/json3/lib/json3.min.js"></script>
        <![endif]-->
        <link href='/css/fonts/SourceSansPro.css' rel='stylesheet' type='text/css'/>
        <link href='/css/fonts/HelveticaNeue.css' rel='stylesheet' type='text/css'/>
        <link rel=" icon" href="/favicon.png" />
        <script src="/$ctxlang$/localization/$versioncode$.js"  type="text/javascript"></script>
        <script type="text/javascript"> window.versioncode = "$versioncode$" </script>

        $if(production)$
        <link href='/$versioncode$.all-styling-minified.css' rel='stylesheet' type='text/css'/>
        <script src="/$versioncode$.optimized-system.js"></script>
        $else$
        <link href='/less/less-compiled.css' rel='stylesheet' type='text/css'/>
        <link rel="stylesheet" href="/less/regular-css-compiled.css" />
        <script src="/compiled_jsx/requirejs_config.js"></script>
        <script src="/bower_components/requirejs/require.js"></script>
        $endif$

        $analyticsLoaderKontrakcja()$
####
analyticsLoaderBase=
  // This is used both by frontend project, i.e. paths served by Backbone.Router and kontrakcja

  // start Mixpanel
(function(e,b){if(!b.__SV){var a,f,i,g;window.mixpanel=b;a=e.createElement("script");a.type="text/javascript";a.async=!0;a.src=("https:"===e.location.protocol?"https:":"http:")+'//cdn.mxpnl.com/libs/mixpanel-2.2.min.js';f=e.getElementsByTagName("script")[0];f.parentNode.insertBefore(a,f);b._i=[];b.init=function(a,e,d){function f(b,h){var a=h.split(".");2==a.length&&(b=b[a[0]],h=a[1]);b[h]=function(){b.push([h].concat(Array.prototype.slice.call(arguments,0)))}}var c=b;"undefined"!==
typeof d?c=b[d]=[]:d="mixpanel";c.people=c.people||[];c.toString=function(b){var a="mixpanel";"mixpanel"!==d&&(a+="."+d);b||(a+=" (stub)");return a};c.people.toString=function(){return c.toString(1)+".people (stub)"};i="disable track track_pageview track_links track_forms register register_once alias unregister identify name_tag set_config people.set people.increment".split(" ");for(g=0;g<i.length;g++)f(c,i[g]);b._i.push([a,e,d])};b.__SV=1.2}})(document,window.mixpanel||[]);

        mixpanel.init("$analytics.token$");

        $if(analytics.userid)$
          mixpanel.identify("$analytics.userid$");
        $endif$

        $if(analytics.properties)$
        var analyticsproperties = $analytics.properties:noescape()$;

          mixpanel.register(analyticsproperties);
          mixpanel.people.set(analyticsproperties);
        if(analyticsproperties['Smart Name'])
          mixpanel.name_tag(analyticsproperties['Smart Name']);
        $endif$
   // end Mixpanel

  // Google analytics tracking
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', '$analytics.tokenGa$']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
// end Google analytics
####
analyticsLoaderKontrakcja=
<script>
  $analyticsLoaderBase()$
  mixpanel.track_links('.js-logout', 'Click Logout');
  mixpanel.track_links('#logo', 'Click Logo');
  mixpanel.track_links('.fromtemplate', 'Click from template');
</script>
###
pageFooter=
<style>
$if(custombarscolour)$
  footer.site {
      background: $custombarscolour$ !important;
  }
$endif$


$if(custombarshighlightcolour)$
  footer.site {
      border-top-color: $custombarshighlightcolour$;
  }
$endif$

$if(custombarstextcolour)$
    footer.site .poweredbyscrive {
      color: $custombarstextcolour$;
      text-shadow: none;
    }
$endif$


</style>

<!-- Styling of the design view footer -->
<style>
$if(custombarscolour)$
  .design-view-button-bar {
      background: $custombarscolour$ !important;
  }
$endif$
</style>

$if(logged)$

<footer class="site $if(thin)$thin$endif$ logged-in">
    <div class="poweredbyscrive">
        $if(!custombarscolour)$
        <div class='text'>Powered by <div class='logo'/></div>
        $else$
        <div class='text'>Powered by Scrive</div>
        $endif$
      </div>
  $postPageCode()$
</footer>

$else$
<footer class="site thin">
  <div class="ct $if(thin)$thin$endif$ $if(!logged)$thin$endif$">

    <div class="visions">
      <p>E-sign with peace of mind.</p>
    </div>

    <nav>
      <header><h4>$_pageFooterExplore()$</h4></header>
      <div class="content">
        <ul class="tree">
          <li class="branch"><a href="$httplink$$langprefix()$api" id="nav-api"> $_pageFooterAPI()$</a></li>
          <li class="branch"><a href="$httplink$$langprefix()$jobs" id="nav-jobs"> $_pageFooterWork()$</a></li>
          <li class="branch"><a href="$langprefix()$verify" id="nav-privacy">$_footerVerifyDocument()$</a></li>
          <li class="branch"><a href="$httplink$$langprefix()$get-started">$_headerGetStarted()$</a></li>
        </ul>
      </div>
    </nav>

    <nav>
      <header><h4>$_pageFooterTerms()$</h4></header>
      <div class="content">
        <ul class="tree">
          <li class="branch"><a href="$httplink$$langprefix()$terms" id="nav-terms">$_pageFooterTOS()$</a></li>
          <li class="branch"><a href="$httplink$$langprefix()$environment" id="nav-environment">$_pageFooterEnvironmentalPolicy()$</a></li>
          <li class="branch"><a href="$httplink$$langprefix()$privacy" id="nav-privacy">$_pageFooterPrivacyPolicy()$</a></li>
        </ul>
      </div>
    </nav>

    <nav>
      <header><h4>$_pageFooterContact()$</h4></header>
      <div class="content">
          <ul class="tree">
              <li class="branch">
                  $_pageFooterWorkDay()$
              </li>
              <li class="branch">
                  <a href="tel:+46851977900">+46851977900</a>
              </li>
              <li class="branch">
                  <a href="mailto:info@scrive.com">info@scrive.com</a>
              </li>
          </ul>
      </div>
  </nav>

  <nav>
      <header><h4>$_pageFooterAddress()$</h4></header>
      <div class="content">
          <ul class="tree">
              <li class="branch">$_pageFooterAddress1()$</li>
              <li class="branch">$_pageFooterAddress2()$</li>
              <li class="branch">$_pageFooterAddress3()$</li>
              <li class="branch">$_pageFooterAddress4()$</li>
          </ul>
      </div>
  </nav>

    <nav class="last">
      <div class="content">
        <ul class="icons">
          <li>
            <a href="http://www.linkedin.com/company/scrive" target="_blank">
              <img src="/img/sm-linkedin.png" width="32" height="32" alt="LinkedIn" />
            </a>
          </li>
          <li>
            <a href="https://twitter.com/scrive" target="_blank">
              <img src="/img/sm-twitter.png" width="32" height="32" alt="Twitter" />
            </a>
          </li>
          <li class="last">
            <a href="https://www.facebook.com/pages/Scrive/237391196280189" target="_blank">
              <img src="/img/sm-facebook.png" width="32" height="32" alt="Facebook" />
            </a>
          </li>
        </ul>
        <ul class="tree">
          <li class="branch">
            $langSwitcher()$
          </li>
        </ul>
      </div>
    </nav>
  </div>
  $postPageCode()$
</footer>
$endif$
####
postPageCode=

  <!-- Google Code for Tagg f&ouml;r ommarknadsf&ouml;ring -->
  <!-- Remarketing tags may not be associated with personally identifiable information or placed on pages related to sensitive categories. For instructions on adding this tag and more information on the above requirements, read the setup guide: google.com/ads/remarketingsetup -->
  <script type="text/javascript">

  /* <![CDATA[ */
  var google_conversion_id = 1009530676;
  var google_conversion_label = "bYPcCKyX6QMQtO6w4QM";
  var google_custom_params = window.google_tag_params;
  var google_remarketing_only = true;
  /* ]]> */
  </script>
  <script type="text/javascript" src="//www.googleadservices.com/pagead/conversion.js"></script>
  <script>
   require(['jquery'], function(jQuery) {
     var container = jQuery('.mainContainer');
     // Fill the whole page
     if ($if(logged)$true$else$false$endif$ && container.find('.short-input-section').length > 0) {
       container.height(jQuery(window).height() - jQuery('header').height() - jQuery('footer').height());
     }
   });
  </script>
  <noscript>
  <div style="display:inline;">
  <img height="1" width="1" style="border-style:none;" alt="" src="//googleads.g.doubleclick.net/pagead/viewthroughconversion/1009530676/?value=0&amp;label=bYPcCKyX6QMQtO6w4QM&amp;guid=ON&amp;script=0"/>
  </div>
  </noscript>

###
langprefix=/$langcode$/
####
header=
$if(logged)$
<script>

 require(['Backbone', 'legacy_code'], function() {
   window.BlockingInfo = Blocking();
 });
</script>
$endif$

<style>
$if(custombarshighlightcolour)$
header.site nav ul.ct li a.page:hover {
    border-top-color: $custombarshighlightcolour$;
    color: $custombarshighlightcolour$;
}
header.site nav {
    border-top-color: $custombarshighlightcolour$;
    border-bottom-color:$custombarshighlightcolour$;
}

header.site nav ul.ct li {
    border-color: $custombarshighlightcolour$;
}
$endif$

$if(custombarstextcolour)$
  header.site nav ul.ct li a.page  {
    color: $custombarstextcolour$;
    text-shadow: none;
  }

  header.site nav ul.ct li a.page.active  {
    color: $custombarstextcolour$;
  }

  header.site nav ul.ct li a.page:hover  {
    color: $custombarstextcolour$;
  }
$endif$


$if(custombarscolour)$
  header.site {
    background: none repeat scroll 0 0 $custombarscolour$;
    border-top-color:$custombarscolour$;
  }

  div.design-view-button-bar {
    background: $custombarscolour$;
  }
$endif$

</style>



<header class="site $if(!logged)$thin$endif$">
  <nav>
    <ul class="ct $if(!logged)$thin$endif$">
      <li id="branding">
        <a id="logo" class="page" href="$httplink$$langprefix()$"><img $if(customlogo)$ src="$customlogolink$"  style="margin:0px" $else$ src="/img/logo.png" width="120" height="23" $endif$ /></a>
        <div id="tagline" class="h3"></div>
      </li>
      $if(logged)$
      <ul class="right-container">
        <li class="float-right" >
          <a class="page js-logout"  href="$linklogout$">$_headerLogOut()$</a>
        </li>
        <li class="float-right" >
            <a class="page $if(account)$active$endif$" id='page-account'  href="$linkaccount$">$_headerAccount()$</a>
        </li>
        <li class="page-first float-right" >
            <a class="s-archive page $if(archive)$active$endif$ "  id='page-archive' href="$linkissue$">$_headerArchive()$</a>
        </li>
        <li class="session-create float-right fromtemplate2">
            <a class="button button fromtemplate" onclick='createfromtemplate(event)' href="/fromtemplate">$_headerTemplate()$ </a>
        </li>
        <li class="session-create float-right">
            <a class="button button js-create-document"            onclick='createnewdocument(event)' href="/newdocument">$_headerSend()$ </a>
        </li>
      </ul>
      $else$
      <li class="session-login float-right">
        <a class="button" id="page-signin" href="$langprefix()$login">$_login()$ </a>
        <a class="button" id="page-signup" href="$langprefix()$signup">$_startFreeTrial()$</a>
      </li>
      <li class="float-right"  >
          <a class="page"   href="$httplink$$langprefix()$contact">$_headerContakt()$</a>
      </li>
      <li class="float-right" >
          <a class="page"  href="$httplink$$langprefix()$about">$_headerAbout()$</a>
      </li>
      <li class="float-right" >
          <a class="page"  href="$httplink$$langprefix()$cases">$_headerCases()$</a>
      </li>
      <li class="float-right" >
          <a class="page price-plan-page-link"  href="$httplink$$langprefix()$pricing">$_headerPricing()$</a>
      </li>
      <li class="float-right" >
          <a class="page"  href="$httplink$$langprefix()$legal">$_headerLegal()$</a>
      </li>
      <li class="float-right" >
          <a class="page"  href="$httplink$$langprefix()$features">$_headerFeatures()$</a>
      </li>
      $endif$
    </ul>
  </nav>
</header>
####
langSwitcher=
<div class='langSwitcher'></div>
<script>
 require(['jquery', 'Backbone', 'legacy_code'], function(jQuery) {
              jQuery(function() {
                   var changeLang = function(l) {
                      mixpanel.track('Click switch languages', { 'Language' : l });
                      $if(logged)$
                        Language.changeForPageAndUserAndReload(l);
                      $else$
                        Language.changeForPageAndReload(l);
                      $endif$
                   }
                   var languages = [
                      {value : "en", name : "$EnglishInEnglish()$"}
                    , {value : "sv", name : "$SwedishInSwedish()$"}
                    , {value : "de", name : "$GermanInGerman()$"}
                    , {value : "fr", name : "$FrenchInFrench()$"}
                    , {value : "it", name : "$ItalianInItalian()$"}
                    , {value : "es", name : "$SpanishInSpanish()$"}
                    , {value : "pt", name : "$PortugueseInPortuguese()$"}
                    , {value : "nl", name : "$DutchInDutch()$"}
                    , {value : "da", name : "$DanishInDanish()$"}
                    , {value : "no", name : "$NorwegianInNorwegian()$"}
                    , {value : "el", name : "$GreekInGreek()$"}
                   ]
                   languages = _.sortBy(languages, function(l) {return l.name;});
                   var lname = _.findWhere(languages, {value : "$langcode$"}).name;

                   var select = new Select({  name : lname,
                                              cssClass: "change-language",
                                              textWidth: 80,
                                              border: "none",
                                              onSelect : function(v) {changeLang(v);},
                                              adjustHeightOnExpand : true,
                                              options: _.filter(languages, function(l) { return l.value != "$langcode$" && !l.hidden; })
                                 })
                   jQuery('.langSwitcher').append(select.el());
              })
});
</script>
####
contactUsVivocha=
<aside class="contact">
  <nav class="contact">
  <ul>
    <li class="call">
      <a class="" href="tel:+46851977900">
        <div class="icon"><img src="/img/phone.png" width="32" height="32" alt="Call"/></div>
        <div class="hover">
          <p>Call us now</p>
          <small>+46 8 519 779 00</small>
        </div>
      </a>
    </li>
    <li class="email">
      <a class="" href="mailto:info@scrive.com">
        <div class="icon"><img src="/img/mail-us.png" width="32" height="32" alt="Email"/></div>
        <div class="hover">
          <p>Send an email</p>
          <small>info@scrive.com</small>
        </div>
      </a>
    </li>
    <li class="chat-vivocha vivocha_widget_openmedia_chat">
      <a class="" href="#vivocha-chat-url">
        <div class="icon"><img src="/img/chat.png" width="32" height="32" alt="Chat"/></div>
        <div class="hover">
          <p>Chat now</p>
          <small>Live chat</small>
        </div>
      </a>
    </li>
  </ul>
</nav>
</aside>
$loadScript(src="https://www.vivocha.com/a/scrive/api/vivocha.js")$
####
loadScript=
<script>
 require(['jquery'], function(jQuery) {
  jQuery(function(){
    var script = document.createElement( 'script' );
    script.type = 'text/javascript';
    script.src = "$src$";
    jQuery("head").append( script );
  })
});
</script>
####
