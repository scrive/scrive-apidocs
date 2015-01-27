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
signSessionTimeOut=
<div class='mainContainer big'>
  <img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
  <div class='short-input-section'>
    <div class="shadowed">
     $_signSessionTimedOut()$
    </div>
  </div>
</div>
<script>
 mixpanel.track('Sign Session Timeout', {URL : window.location.href});
</script>
###
sessionTimeOut=
<div class='mainContainer big'>
  <img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
  <div class='short-input-section'>
    <div class="shadowed">
      <h1>$_sessionTimeOutHeader()$</h1>
      <h4>$_sessionTimeOutContent1()$</h4>
      <h4>$_sessionTimeOutContent2()$</h4>
    </div>
  </div>
</div>
<script>
  require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(document).ready(function() {
      var cookies = Cookies.buildCookieMap();
      var cookieNames = [];
      for(var key in cookies) {
        cookieNames.push(key);
      }
      mixpanel.track('Cookies enabled but timeouted', {'cookie': cookieNames.toString(),
                                                       'user-agent': navigator.userAgent});
    });
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
	<script src="/bower_components/es5-shim/es5-shim.js"></script>
	<script src="/bower_components/es5-shim/es5-sham.js"></script>
        <!--[if lt IE 9]>
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

          require(['Underscore'], function(_) {
            // Overwrite with nicer values, like "Proffice & Foo" instead of "Proffice &amp; Foo"
            var nicePropList = _.map(analyticsproperties, function(value, key) {
              return [key, _.unescape(value)];
            });
            analyticsproperties = _.object(nicePropList);

            console.log(analyticsproperties);

            mixpanel.register(analyticsproperties);
            mixpanel.people.set(analyticsproperties);

            if(analyticsproperties['Smart Name'])
              mixpanel.name_tag(analyticsproperties['Smart Name']);
          });

        $endif$
   // end Mixpanel

   // Hubspot configuration
   window.hubspotConf = $analytics.hubspotConf:noescape()$;

  // Google analytics tracking
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', '$analytics.tokenGa$']);
  $if(analytics.userid)$
    // Mark this visitor as a user in GA, in slot 2 (also on session)
    _gaq.push(['_setCustomVar', 2, 'User?', 'true', 2]);
  $endif$
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
// end Google analytics

// HubSpot js: Start of Async HubSpot Analytics Code
  (function(d,s,i,r) {
    if (d.getElementById(i)){return;}
    var n=d.createElement(s),e=d.getElementsByTagName(s)[0];
    n.id=i;n.src='//js.hs-analytics.net/analytics/'+(Math.ceil(new Date()/r)*r)+'/431732.js';
    e.parentNode.insertBefore(n, e);
  })(document,"script","hs-analytics",300000);
// end Hubspot js
####
analyticsLoaderKontrakcja=
$loadScript(src="http://js.hubspot.com/forms/current.js")$
<script>
  $analyticsLoaderBase()$
  mixpanel.track_links('#logo', 'Click Logo');
</script>
###
pageFooter=
<div class='place-for-footer'>
  $postPageCode()$
</div>
<script>
require(['jquery', 'React' , 'pages/footer','legacy_code'], function(jQuery,React,Footer) {

  jQuery(function(){
    React.renderComponent(
      Footer({
        logged: $if(logged)$ true $else$ false $endif$,
        thin : $if(thin)$ true $else$ false $endif$,
        httplink : "$httplink$",
        langprefix : "$langprefix()$",
        color :  "$custombarscolour$",
        textcolor :  "$custombarstextcolour$",
        highlightcolor : "$custombarshighlightcolour$"
      })
    , jQuery('.place-for-footer')[0]);
  });

});
</script>
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
#### Style part below is just to stop pushing content down on login page (before rendering). You can remove it when whole this is in react.
####
header=
$headerStyles()$
$headerAddons()$
<div class='place-for-header' $if(!logged)$ style="height:81px;background-color: #495259" $endif$>
  $postPageCode()$
</div>
<script>
require(['jquery', 'React' , 'pages/header','legacy_code'], function(jQuery,React,Header) {

  jQuery(function(){
    React.renderComponent(
      Header({
        logged: $if(logged)$ true $else$ false $endif$,
        customlogourl : "$customlogolink$",
        httplink : "$httplink$",
        langprefix : "$langprefix()$",
        archive : $if(archive)$true$else$false$endif$,
        account : $if(account)$true$else$false$endif$
      })
    , jQuery('.place-for-header')[0]);
  });

});
</script>
####
headerStyles=
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
####
headerAddons=
$if(logged)$
<script>

 require(['Backbone', 'legacy_code'], function() {
   window.BlockingInfo = Blocking();
 });
</script>
$endif$
####
contactUsVivocha=
<aside class="contact">
  <nav class="contact">
  <ul>
    <li class="call">
      <a class="" href="tel:+46851977900">
        <div class="icon"><img src="/img/phone.png" width="32" height="32" alt="Call"/></div>
        <div class="hover">
          <p>$_vivochaCallUsNow()$</p>
          <small>+46 8 519 779 00</small>
        </div>
      </a>
    </li>
    <li class="email">
      <a class="" href="mailto:info@scrive.com">
        <div class="icon"><img src="/img/mail-us.png" width="32" height="32" alt="Email"/></div>
        <div class="hover">
          <p>$_vivochaSendEmail()$</p>
          <small>info@scrive.com</small>
        </div>
      </a>
    </li>
    <li class="chat-vivocha vivocha_widget_openmedia_chat">
      <a class="" href="#vivocha-chat-url">
        <div class="icon"><img src="/img/chat.png" width="32" height="32" alt="Chat"/></div>
        <div class="hover">
          <p>$_vivochaChatNow()$</p>
          <small>$_vivochaLiveChat()$</small>
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
