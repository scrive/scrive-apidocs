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
       <div class="clearfix"></div>
       <div class="spacer40"></div>
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
        <link href='https://fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css"/>
        <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:200,400,700,200italic,400italic,700italic' rel='stylesheet' type='text/css'>
        <script src="/localization/$ctxlang$/$versioncode$.js"  type="text/javascript"></script>
        $staticResources:noescape()$
####
####
pageFooter=

<footer class="site">
  <div class="ct">
    <div class="">
      <nav>
        <ul>
          <li><a href="{path='privacy'}">Privacy</a></li>
          <li><a href="{path='terms'}">Privacy</a></li>
          <li><a href="{path='jobs'}">Privacy</a></li>
          <li><a href="{path='api'}">Privacy</a></li>
          <li><a href="{path='labs'}">Privacy</a></li>
          <li class='last'><a href="{path='blog'}">Privacy</a></li>
        </ul>
      </nav>
      <ul class="etc">
        <li>Copyright &copy; 2010-2012 Scrive AB.</li>
        <li>Version bla bla bla</li>
      </ul>
      <ul class="social">
        <li>
          <script type="text/javascript" src="https://apis.google.com/js/plusone.js"></script>
          <div class="g-plusone" data-size="medium"></div>
        </li>
        <li>
          <a href="https://twitter.com/share" class="twitter-share-button" data-lang="en">Tweet</a>
          <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0];if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src="https://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
        </li>
        <li>
          <div id="fb-root"></div>
          <script>(function(d, s, id) {
            var js, fjs = d.getElementsByTagName(s)[0];
            if (d.getElementById(id)) return;
            js = d.createElement(s); js.id = id;
            js.src = "//connect.facebook.net/en_US/all.js#xfbml=1";
            fjs.parentNode.insertBefore(js, fjs);
          }(document, 'script', 'facebook-jssdk'));</script>
          <div class="fb-like" data-href="http://scrive.com" data-send="true" data-layout="button_count" data-width="80" data-show-faces="false" data-font="arial"></div>
        </li>
      </ul>
    </div>
  </div>
</footer>

<!--


<div id="footer-container" class="newdesign">
  <div class="top-border"></div>
  <div class="spacer"></div>
  <div class="content">
    <div class="col left">
      <strong>$_pageFooterTheService()$</strong>
      <ul>
        <li><a href="$linkpriceplan$">$_pageFooterPrices()$</a></li>
      </ul>
      <br />
      <strong>$_pageFooterTerms()$</strong>
      <ul>
        <li><a href="$linksecurity$">$_pageFooterSecurity()$</a></li>
        <li><a href="$linklegal$">$_pageFooterLaw()$</a></li>
        <li><a href="$linkprivacypolicy$">$_pageFooterPrivacy()$</a></li>
        <li><a href="$linkterms$">$_pageFooterTOS()$</a></li>
      </ul>
      <br />
      <strong style='text-transform:uppercase'>Scrive</strong>
      <ul>
        <li><a href="$linkhome$">$_pageFooterHome()$</a></li>
        <li><a href="$linkabout$">$_pageFooterAbout()$</a></li>
        <li><a href="http://blog.scrive.com/jobs/">$_pageFooterJobs()$</a></li>
        <li><a href="$linkpartners$">$_pageFooterPartners()$</a></li>
        <li><a href="$linkclients$">$_pageFooterCustomers()$</a></li>
        <li><a href="$linkcontactus$">$_pageFooterContactUs()$</a></li>
      </ul>
    </div>
    $if(publicsafe)$
    <div class="col centre">
      <strong>$_pageFooterTwitter()$</strong>
      <div class="tweet">

      </div>
    </div>
    $endif$
    <div class="col right">
      $_pageFooterContact1()$
      <ul>
        <li>
          $_pageFooterContact2()$
        </li>
        <li>
          $_pageFooterContact3()$
        </li>
        <li>
          $_pageFooterContact4()$
        </li>
        <li>
          $_pageFooterContact5()$
        </li>
        <li>
          $_pageFooterContact6()$
        </li>
        <li>
          $_pageFooterContact7()$
        </li>
        <li>
          $_pageFooterContact8()$
        </li>
      </ul>
    </div>
  </div>
</div>

<div class="imgCache">
  <img src="/theme/../img/spr-flash-bg.png" />
  <img src="/img/spr-flash-bg.png" />
</div>
-->
####
header=
 <script>
   window.BlockingInfo = Blocking();
 </script>


<header class="site">
  <nav>
    <ul class="ct">
      <li id="branding">
        <a id="logo" class="page" href="index.html"><img src="/img/logo.png" width="120" height="22"></a>
        <div id="tagline" class="h3"></div>
      </li>
      $if(logged)$
      <li class="session-logout">
        <a class="button button-small button-red" id="page-signup" href="$linklogout$">$_headerLogOut()$</a>
      </li>
      <li><a class="page" id="page-about" href="$linkaccount$">$_headerAccount()$</a></li>
      <li><a class="page" id="page-cases" href="$linkissue$">$_headerArchive()$</a></li>
      <li class="session-create">
        <a class="button button-small button-red" onclick='createfromtemplate()' href="#">$_headerTemplate()$ </a>
      </li>
      <li class="session-create">
        <a class="button button-small"            onclick='createnewdocument()' href="#">$_headerSend()$ </a>
      </li>
      $else$
      <li class="session-login">
        <a class="button button-small" id="page-login" href="/login">Log in </a>
      </li>
      <li><a class="page" id="page-pricing" href="{path=pricing}">Features</a></li>
      <li><a class="page" id="page-legal" href="{path=legal}">Features</a></li>
      <li><a class="page" id="page-cases" href="{path=cases}">Features</a></li>
      <li><a class="page" id="page-about" href="{path=about}">Features</a></li>
      <li class="page-first"><a class="page" id="page-features" href="{path=features}">Features</a></li>
      $endif$
    </ul>
  </nav>
</header>


<!--
 <div id="top-slim-container" class="newdesign">

                        <div class="content">
                                <a href="$linkhome$"
                                class="scrive-logo"></a>


                                $if(logged)$

                               <a href="$linklogout$" class="toplink logout">$_headerLogOut()$</a>
                               <a href="$linkaccount$" class="toplink s-account">$_headerAccount()$</a>
                               <a href="$linkissue$" class="toplink">$_headerArchive()$</a>
                               <span class='toplink btn'><a class="button button-small" onclick='createfromtemplate()' ><div class="label">$_headerTemplate()$</div></a></span>
                               <span class='toplink btn'><a class="button button-small s-create-document"  onclick='createnewdocument()'><div class="label">$_headerSend()$</div></a></span>
                                $langSwitcher()$
                                $else$
                                <span id="moveleft2">
                                    <a href="#" class="login-button"><p>$_login()$</p></a>
                                    <a href="$linkcontactus$" class="toplink">$_pageHeaderContactUs()$</a>
                                    <a href="$linkabout$" class="toplink">$_pageFooterAbout()$</a>
                                    <a href="$linkclients$" class="toplink">$_customerCases()$</a>
                                    <a href="$linkpriceplan$" class="toplink">$_pageFooterPrices()$</a>
                                $langSwitcher()$
                                  </span>

                                $endif$

                            </div>


            <div class="clearboth"></div>
          </div>
-->
####
langSwitcher=
  <div class="langSwitcher">
    $if(!logged)$
    <div><a href="$linksv$" class="icon flag swedish $if(langswedish)$active$endif$"></a></div>
    <div><a href="$linken$" class="icon flag british $if(langenglish)$active$endif$"></a></div>
    $else$
    <div><form action="$linklangswitch$" method="POST">
           <input type="hidden" name="lang" value="LANG_SV"/>
           $if(linksv)$
           <input type="hidden" name="referer" value="$linksv$"/>
           $endif$
           <a href="" class="submit icon flag swedish $if(langswedish)$active$endif$"></a>
         </form></div>
    <div><form action="$linklangswitch$" method="POST">
           <input type="hidden" name="lang" value="LANG_EN"/>
           $if(linkgben)$
           <input type="hidden" name="referer" value="$linkgben$"/>
           $endif$
           <a href="" class="submit icon flag british $if(langenglish)$active$endif$"></a>
         </form></div>
    $endif$
  </div>
####
