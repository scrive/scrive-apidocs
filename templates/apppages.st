###
notFound=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<div class='short-input-section'>
  <header>
    $_notFound()$
  </header>
  <div class='short-input-container'>
    <div class='short-input-container-body-wrapper'>
      <div class='short-input-container-body'>
           $_afterErrorGoHome()$
      </div>
    </div>
  </div>
</div>
</div>
###
internalServerError=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<div class='short-input-section'>
  <header>
   $_internalServerError()$
  </header>
  <div class='short-input-container'>
    <div class='short-input-container-body-wrapper'>
      <div class='short-input-container-body'>
           $_afterErrorGoHome()$
      </div>
    </div>
  </div>
</div>
</div>
###
wholePage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
      <body>
       $header()$
       $flashmessages()$
       $content:noescape()$
       $pageFooter()$
      </body>
    </html>

####
pagetitle=$title$$if(production)$$else$ (devel) $endif$
####
doctype=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
####
standardHeaderContents=
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <meta http-equiv="content-language" content="$ctxlang$" />
        <!--[if lt IE 9]>
        <script src="/libs/html5shiv.js"></script>
        <![endif]-->
        <link href='/css/fonts/SourceSansPro.css' rel='stylesheet' type='text/css'/>
        <link href='/css/fonts/HelveticaNeue.css' rel='stylesheet' type='text/css'/>
        <script src="/$ctxlang$/localization/$versioncode$.js"  type="text/javascript"></script>
        $staticResources:noescape()$

<!-- start Mixpanel -->
        <script>
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

        mixpanel.track_links('.logout', 'Click Logout');
        mixpanel.track_links('#logo', 'Click Logo');
        mixpanel.track_links('.fromtemplate', 'Click from template');

        </script>
<!-- end Mixpanel -->

####
pageFooter=
<footer class="site">
  <div class="ct $if(thin)$thin$endif$ $if(!logged)$thin$endif$">


    <div class="visions">
      <p>Visions made in Sweden. Built by experts from around the
        world. </p>
    </div>

    <nav class="left">
      <div class="content">
        <ul class="icons">
          <li>
            <a href="https://plus.google.com/share?url=http://scrive.com" target="_blank">
              <img src="/img/sm-google.png" width="32" height="32" alt="Google+" />
            </a>
          </li>
          <li>
            <a href="https://twitter.com/scrive" target="_blank">
              <img src="/img/sm-twitter.png" width="32" height="32" alt="Twitter" />
            </a>
          </li>
          <li>
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


    <nav>
      <header>
        <h4>$_pageFooterExplore()$</h4>
      </header>
      <div class="content">
        <ul class="tree">
          <li class="branch">
            <a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$api" id="nav-api">
              $_pageFooterAPI()$
            </a>
          </li>

          <li class="branch">
            <a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$labs" id="nav-labs">
              $_pageFooterLabs()$
            </a>
          </li>
          <li class="branch">
            <a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$blog" id="nav-blog">
              $_pageFooterBlog()$
            </a>
          </li>
          <li class="branch">
            <a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$jobs" id="nav-jobs">
              $_pageFooterWork()$
            </a>
          </li>
        </ul>
      </div>
    </nav>

    <nav>
      <header>
        <h4>$_pageFooterTerms()$</h4>
      </header>
      <div class="content">
        <ul class="tree">
          <li class="branch"><a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$terms" id="nav-terms">$_pageFooterTOS()$</a></li>
          <li class="branch"><a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$environment" id="nav-environment">$_pageFooterEnvironmentalPolicy()$</a></li>
          <li class="branch"><a href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$privacy" id="nav-privacy">$_pageFooterPrivacyPolicy()$</a></li>
        </ul>
      </div>
    </nav>

    <nav>
      <header>
        <h4>$_pageFooterContact()$</h4>
      </header>
      <div class="content">
        <ul class="tree">
          <li class="branch">
            $_pageFooterWorkDay()$
          </li>
          <li class="branch">
            <a href="tel:$_pageFooterPhone()$">
              $_pageFooterPhone()$
            </a>
          <li class="branch">
            <a href="mailto:$_pageFooterMail()$">
              $_pageFooterMail()$
            </a>
          </li>
          </li>
        </ul>
      </div>
    </nav>

    <nav>
      <header>
        <h4>$_pageFooterAddress()$</h4>
      </header>
      <div class="content">
        <ul class="tree">
          <li class="branch">$_pageFooterAddress1()$</li>
          <li class="branch">$_pageFooterAddress2()$</li>
          <li class="branch">$_pageFooterAddress3()$</li>
          <li class="branch">$_pageFooterAddress4()$</li>
        </ul>
      </div>
    </nav>
  </div>
</footer>
####
header=
<script>
  window.BlockingInfo = Blocking();
</script>

<header class="site $if(thin)$thin$endif$ $if(!logged)$thin$endif$">
  <nav>
    <ul class="ct $if(thin)$thin$endif$ $if(!logged)$thin$endif$">
      <li id="branding">
        <a id="logo" class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$"><img src="/img/logo.png" width="120" height="22"/></a>
        <div id="tagline" class="h3"></div>
      </li>
      $if(logged)$
      <li class="float-right">
        <a class="page js-logout" href="$linklogout$">$_headerLogOut()$</a>
      </li>
      <li class="float-right"><a class="page" id='page-account' href="$linkaccount$">$_headerAccount()$</a></li>
      <li class="page-first float-right"><a class=" page" id='page-archive' href="$linkissue$">$_headerArchive()$</a></li>
      <li class="session-create float-right">
        <a class="button button-small fromtemplate" onclick='createfromtemplate(event)' href="/fromtemplate">$_headerTemplate()$ </a>
      </li>
      <li class="session-create float-right">
        <a class="button button-small js-create-document"            onclick='createnewdocument(event)' href="/newdocument">$_headerSend()$ </a>
      </li>
      $else$
      <li class="page-first float-left"><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$features">$_headerFeatures()$</a></li>
      <li class="float-left"><a class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$pricing">$_headerPricing()$</a></li>
      <li class="float-left"><a class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$legal">$_headerLegal()$</a></li>
      <li class="float-left"><a class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$cases">$_headerCases()$</a></li>
      <li class="float-left"><a class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$about">$_headerAbout()$</a></li>
      <li class="float-left"><a class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$contact">$_headerContakt()$</a></li>
      <li class="session-login float-right">
        <a class="button button-small" id="page-login" href="/login">$_login()$ </a>
        <a class="button button-small button-red" id="page-signup" href="$if(langenglish)$/en/$else$/sv/$endif$signup">$_signUp()$</a>
      </li>
      $endif$
    </ul>
  </nav>
</header>
####
langSwitcher=
<div class='langSwitcher'></div>
<script>
              jQuery(function() {
                   var changeLang = function(l) {
                      mixpanel.track('Click switch languages', { 'Language' : l });
                      new Submit({
                          method : "POST",
                          url: "$linklangswitch$",
                          lang: l == "en" ? "LANG_EN" : "LANG_SV"
                                }).sendAjax(function() {
                                   var p = window.location.pathname;
                                   if (p.substring(0, 3) == "/en" || p.substring(0, 3) == "/sv") {
                                        p = "/" + l + p.substr(3);
                                        window.location.pathname = p;
                                   } else
                                      window.location.reload();
                          })
                   }

                   var select = new Select({  name : $if(langswedish)$ "$_Swedish()$" $else$ "$_English()$" $endif$,
                                              textWidth: 80,
                                              options: [ $if(langswedish)$
                                                               {name: "$_English()$", value: "en", onSelect : function() {changeLang("en");}}
                                                         $else$
                                                               {name: "$_Swedish()$", value: "sv", onSelect : function() {changeLang("sv");} }
                                                         $endif$
                                                ]})
                   jQuery('.langSwitcher').append(select.view().el);
              })
</script>
####
