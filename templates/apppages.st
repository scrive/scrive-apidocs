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
pageFooter=
<footer class="site">
  <div class="ct">
      <nav class='top'>
        <ul>
          <li class="copyright"><a href="$if(langenglish)$/en/$else$/sv/$endif$">&copy; $_footerScrive()$</a></li>
          <li><a href="$if(langenglish)$/en/$else$/sv/$endif$blog">$_footerBlog()$</a></li>
          <li><a href="$if(langenglish)$/en/$else$/sv/$endif$labs">$_footerLabs()$</a></li>
          <li><a href="$if(langenglish)$/en/$else$/sv/$endif$api">$_footerAPI()$</a></li>
          <li><a href="$if(langenglish)$/en/$else$/sv/$endif$jobs">$_footerJobs()$</a></li>
          <li><a href="$if(langenglish)$/en/$else$/sv/$endif$privacy">$_footerPrivacy()$</a></li>
          <li class='last'><a href="$if(langenglish)$/en/$else$/sv/$endif$terms">$_footerTerms()$</a></li>
          <li class='langSwitcher'>
            <script>
              jQuery(function() {
                   var select = new Select({  name : $if(langswedish)$ "$_Swedish()$" $else$ "$_English()$" $endif$,
                                              options: [ $if(langswedish)$
                                                               {name: "$_English()$", value: "en"}
                                                         $else$
                                                               {name: "$_Swedish()$", value: "sv"}
                                                         $endif$
                                                ]})
                   jQuery('.langSwitcher').append(select.view().el);
              })
            </script>
          </li>
        </ul>

      </nav>
      <nav class='bottom'>
        <ul class="social">
          <li>
            <script type="text/javascript" src="https://apis.google.com/js/plusone.js"></script>
            <div class="g-plusone" data-size="medium"></div>
          </li>
          <li>
            <a href="https://twitter.com/share" class="twitter-share-button" data-lang="en">Tweet</a>
            <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0];if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src="https://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
          </li>
        </ul>
      </nav>
  </div>
</footer>
####
header=
 <script>
   window.BlockingInfo = Blocking();
 </script>

<header class="site">
  <nav>
    <ul class="ct">
      <li id="branding">
        <a id="logo" class="page" href="$if(langenglish)$/en/$else$/sv/$endif$"><img src="/img/logo.png" width="120" height="22"></a>
        <div id="tagline" class="h3"></div>
      </li>
      $if(logged)$
      <li class="session-logout">
        <a class="button button-small button-red" id="page-signup" href="$linklogout$">$_headerLogOut()$</a>
      </li>
      <li><a class="page" id="page-about" href="$linkaccount$">$_headerAccount()$</a></li>
      <li><a class="page" id="page-cases" href="$linkissue$">$_headerArchive()$</a></li>
      <li class="session-create">
        <a class="button button-small" onclick='createfromtemplate()' href="#">$_headerTemplate()$ </a>
      </li>
      <li class="session-create">
        <a class="button button-small"            onclick='createnewdocument()' href="#">$_headerSend()$ </a>
      </li>
      $else$
      <li class="session-login">
        <a class="button button-small" id="page-login" href="/login">Log in </a>
      </li>
      <li><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$contact">$_headerContakt()$</a></li>
      <li><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$about">$_headerAbout()$</a></li>
      <li><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$cases">$_headerCases()$</a></li>
      <li><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$legal">$_headerLegal()$</a></li>
      <li><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$pricing">$_headerPricing()$</a></li>
      <li class="page-first"><a class="page" href="$if(langenglish)$/en/$else$/sv/$endif$features">$_headerFeatures()$</a></li>
      $endif$
    </ul>
  </nav>
</header>
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
