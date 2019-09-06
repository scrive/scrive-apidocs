###
wholePage=$doctype()$
    $htmlopentag()$
      <head>
        <title>$pagetitle()$</title>
        $IECompatibilityMetaTag()$
        $standardHeaderContents()$
      </head>
      <body $if(custombackground)$style="background:$custombackground$;"$endif$>
        <div class="wrapper-position-footer">
         $currentSubscription()$
         $header()$
         $flashmessages()$
         $content:noescape()$
         $pageFooter()$
 	</div>
      </body>
    $htmlclosingtag()$
####
pagetitle=$title$
####
doctype=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
####
htmlclosingtag=
</html>
####
htmlopentag=<html>
####
IECompatibilityMetaTag=<meta http-equiv="X-UA-Compatible" content="IE=edge" />
####
standardHeaderContents=
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <meta http-equiv="content-language" content="$ctxlang$" />
        $if(noviewport)$
        $else$
          <meta name="viewport" content="width=1020" />
        $endif$
        <link rel=" icon" href="$cdnbaseurl$/favicon/$brandingdomainid$/$if(brandinguserid)$$brandinguserid$$else$_$endif$/$brandinghash$" />
        <script type="text/javascript">
          window._trackJs = { token: "$trackjstoken$", enabled: $if(trackjstoken)$ true $else$ false $endif$ };
        </script>
        <script src="$cdnbaseurl$/localization/$versioncode$.$ctxlang$.js"  type="text/javascript" crossorigin="anonymous"></script>
        <script type="text/javascript"> window.versioncode = "$versioncode$" </script>
        $if(!signview)$
          <script type="text/javascript"> window.hubspotConf = $analytics.hubspotConf:noescape()$;</script>
        $endif$
        $if(production)$
        $if(signview)$
        <link href='$cdnbaseurl$/$versioncode$.signview-all-styling-minified.css' rel='stylesheet' type='text/css'/>
        $else$
        <link href='$cdnbaseurl$/$versioncode$.all-styling-minified.css' rel='stylesheet' type='text/css'/>
        $endif$
        $else$
        $if(signview)$
        <link href='/less/signview-less-compiled.css' rel='stylesheet' type='text/css'/>
        $else$
        <link href='/less/less-compiled.css' rel='stylesheet' type='text/css'/>
        $endif$
        $endif$

        $if(signview)$
        <script src="$cdnbaseurl$/compiled/signview-vendor-$versioncode$.js" crossorigin="anonymous"></script>
        $else$
        <script src="$cdnbaseurl$/compiled/vendor-$versioncode$.js" crossorigin="anonymous"></script>
        $endif$
        <link href='$cdnbaseurl$/domain_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>

        $if(nobranding)$
        $else$
          $if(logged)$
          <link href='$cdnbaseurl$/service_branding/$brandingdomainid$/$if(brandinguserid)$$brandinguserid$$else$_$endif$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
            $else$
          <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
          $endif$
        $endif$
        <script>
          window.brandinghash = "$brandinghash$";
          window.brandingdomainid = "$brandingdomainid$";
          window.cdnbaseurl = "$cdnbaseurl$";
          window.brandinguserid = "$brandinguserid$";
        </script>

        $analyticsLoaderKontrakcja()$
####
analyticsLoaderBase=
  // This is used both by frontend project, i.e. paths served by Backbone.Router and kontrakcja

  // start Mixpanel
(function(e,b){if(!b.__SV){var a,f,i,g;window.mixpanel=b;a=e.createElement("script");a.type="text/javascript";a.async=!0;a.src=("https:"===e.location.protocol?"https:":"http:")+'//cdn.mxpnl.com/libs/mixpanel-2.2.min.js';f=e.getElementsByTagName("script")[0];f.parentNode.insertBefore(a,f);b._i=[];b.init=function(a,e,d){function f(b,h){var a=h.split(".");2==a.length&&(b=b[a[0]],h=a[1]);b[h]=function(){b.push([h].concat(Array.prototype.slice.call(arguments,0)))}}var c=b;"undefined"!==
typeof d?c=b[d]=[]:d="mixpanel";c.people=c.people||[];c.toString=function(b){var a="mixpanel";"mixpanel"!==d&&(a+="."+d);b||(a+=" (stub)");return a};c.people.toString=function(){return c.toString(1)+".people (stub)"};i="disable track track_pageview track_links track_forms register register_once alias unregister identify name_tag set_config people.set people.increment".split(" ");for(g=0;g<i.length;g++)f(c,i[g]);b._i.push([a,e,d])};b.__SV=1.2}})(document,window.mixpanel||[]);

        mixpanel.init("$analytics.token$");

        mixpanel.set_config({cross_subdomain_cookie: false, secure_cookie: true});

        $if(analytics.userid)$
          mixpanel.identify("$analytics.userid$");
        $endif$

        $if(analytics.properties)$
          var analyticsproperties = $analytics.properties:noescape()$;

          // Overwrite with nicer values, like "Proffice & Foo" instead of "Proffice &amp; Foo"
          var nicePropList = [];

          var unescapeMap = {'&amp;': '&',
                             '&lt;': '<',
                             '&gt;': '>',
                             '&quot;': '"',
                             '&#x27;': "'",
                             '&#x60;': '`'};

          var escaper = function(match) {
            return unescapeMap[match];
          };

          var source = '(?:&amp;|&lt;|&gt;|&quot;\&#x27;|&#x60;)';
          var testRegexp = RegExp(source);
          var replaceRegexp = RegExp(source, 'g');

          var unescape = function(string) {
            string = string == null ? '' : '' + string;
            return testRegexp.test(string) ? string.replace(replaceRegexp, escaper) : string;
          };

          for (var key in analyticsproperties) {
            nicePropList.push([key, unescape(analyticsproperties[key])]);
          }
          analyticsproperties = {};
          for (var i = 0; i < nicePropList.length; i++) {
            var pair = nicePropList[i];
            analyticsproperties[pair[0]] = pair[1];
          }

          mixpanel.register(analyticsproperties);
          mixpanel.people.set(analyticsproperties);

          if(analyticsproperties['Smart Name'])
            mixpanel.name_tag(analyticsproperties['Smart Name']);

        $endif$
   // end Mixpanel
####
analyticsLoaderKontrakcja=

$if(!signview)$
  $loadScript(src="//js.hubspot.com/forms/current.js")$
  $loadScript(src="//js.hs-analytics.net/analytics/431732.js")$
$endif$

<script>
  $if(analytics.token)$
    $analyticsLoaderBase()$
    mixpanel.track_links('#logo', 'Click Logo');
  $else$
    var mockFunction = function() {};
    window.mixpanel = {
        track : mockFunction
      , register : mockFunction
      , track_links : mockFunction
      , alias : mockFunction
      , identify : mockFunction
      , people : {
        set : mockFunction
      }
    }
  $endif$
</script>
###
pageFooter=
<div class='place-for-footer'>
  $postPageCode()$
</div>
<script>
  var fromTemplateFooter = {
  };
</script>
<script src="$cdnbaseurl$/compiled/all/footer-$versioncode$.js" crossorigin="anonymous"></script>
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
  var google_conversion_format = 3;
  /* ]]> */
  </script>
  <script type="text/javascript" src="//www.googleadservices.com/pagead/conversion.js"></script>
  <script>
     var container = jQuery('.mainContainer');
     // Fill the whole page
     if ($if(logged)$true$else$false$endif$ && container.find('.short-input-section').length > 0) {
       container.height(jQuery(window).height() - jQuery('header').height() - jQuery('footer').height());
     }
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
<div class='place-for-header'>
  $postPageCode()$
</div>
<script>
var fromTemplateHeader = {
  archive : $if(archive)$true$else$false$endif$,
  account : $if(account)$true$else$false$endif$
};
</script>
<script src="$cdnbaseurl$/compiled/all/header-$versioncode$.js" crossorigin="anonymous"></script>
####
loadScript=
<script>
document.addEventListener("DOMContentLoaded", function () {
  var script = document.createElement("script");
  script.type = "text/javascript";
  script.src = "$src$";
  var head = document.head || document.getElementsByTagName("head")[0];
  head.appendChild(script);
});
</script>
####
