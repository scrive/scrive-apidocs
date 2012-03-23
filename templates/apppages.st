###
notFound=<strong>
            $_notFound()$. <a href="/">$_notFoundGoHome()$.</a>
         </strong>

###
pageLogin=
    <div class="centerdivnarrow">
      <p class="headline">$_login()$</p>
      $loginBox()$
    </div>

###
wholePage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
      $standardBody(unwrappedbody=content)$
    </html>

####
pagetitle=$title$$if(production)$$else$ (devel) $endif$
####
doctype=<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
####
standardHeaderContents=
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <meta http-equiv="content-language" content="$ctxlang$" />
        <link href='$protocol$//fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/newdesign.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/designview.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/style.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/calendar.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/signaturedrawer.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/archive.css" media="screen"/>

        <link rel="stylesheet" type="text/css" href="$prefix$/theme/symbols.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/tabs.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/lists.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/buttons.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/account.css" />
        <link href="$prefix$/theme/flash.css" rel="stylesheet" type="text/css" media="screen" />
        <link href="/css/main.css" rel="stylesheet" type="text/css" />
        <link href="$prefix$/theme/docsignview.css" rel="stylesheet" type="text/css" media="screen" />
        <script src="$if(production)$$protocol$//ajax.googleapis.com/ajax/libs/jquery/1.5.1$else$/js$endif$/jquery.min.js"
                type="text/javascript"></script>
        <script src="$if(production)$$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.11$else$/js$endif$/jquery-ui.min.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/jquery.tools.min.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/jquery.MultiFile.js"
                type="text/javascript"></script>
        <script src="$prefix$/tiny_mce/jquery.tinymce.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/json2.js"
                type="text/javascript"></script>
        <script type="text/javascript">
                $javascriptLocalisation()$</script>
        <script src="$prefix$/js/global.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/underscore.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/backbone.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/storage.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/livequery.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/loading.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/flashmessages.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/table.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/infotextinputs.js"
                type="text/javascript"></script>

        <script src="$prefix$/js/buttons.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/uploadbuttons.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/tooltips.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/calendar.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/submits.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/confirmations.js"
                type="text/javascript"></script>
		<script src="$prefix$/js/selects.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/lists.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/tabs.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/files.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/process.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/fields.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/signatories.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/documents.js"
                type="text/javascript"></script>
		<script src="$prefix$/js/confirmationsWithEmails.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/docview.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/docsignviewframes.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/docsignview.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/signatorydesignview.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/docdesignview.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/bankid.js"
                type="text/javascript"></script>
        <script src="/js/jquery.form.js"
                type="text/javascript"></script>
        <script src="/js/wizard.js"
                type="text/javascript"></script>
        <script src="/js/uploadview.js"
                type="text/javascript"></script>
        <script src="/js/attachmentsdesignview.js"
                type="text/javascript"></script>
        <script src="/js/csvsignatorydesign.js"
                type="text/javascript"></script>
        <script src="/js/validation.js"
                type="text/javascript"></script>
        <script src="/js/signaturedrawer.js"
                type="text/javascript"></script>
       <script src="/js/fieldplacementview.js"
                type="text/javascript"></script>
       <script src="/js/companybranding.js"
                type="text/javascript"></script>
       <script src="/js/padview.js"
                type="text/javascript"></script>
       $if(publicsafe)$
        <script src="/js/jquery.tweet.js"
                type="text/javascript"></script>
            $if(production)$
                <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
            $endif$
       $endif$
       <script src="/js/main.js"
                type="text/javascript"></script>
####
standardBodyWithQueryCol=
       $standardBodyWrapper(unwrappedbody=contentsWithQueryCol(unwrappedbody=unwrappedbody))$
####
contentsWithQueryCol=
         <div class="left-column">
           $queryColumn()$
         </div>
         <div class="right-column">
           <div class="mainContainer">
             $unwrappedbody:noescape()$
           </div>
         </div>
####
standardBody=
       $standardBodyWrapper(unwrappedbody=contentsWithoutQueryCol(unwrappedbody=unwrappedbody))$
####
contentsWithoutQueryCol=
         <div class="mainContainer">
           $unwrappedbody:noescape()$
         </div>
####
standardBodyWrapper=
      <body>
       $header()$
        <div class="mainContainer960" >
          $flashmessages()$
          $loginModal()$
        <div class="body-container">
          $unwrappedbody:noescape()$
                                        <div class="clearfix"></div>
                                        <div class="spacer40"></div>
                                        $if(showCreateAccount)$
                                        <div class="try-skrivapa">
                                          <div class="left">
                                            <h3>$_standardBodyWrapperHead()$</h3>
                                            <p>$_standardBodyWrapperBody()$</p>
                                          </div>
                                          <div class="right">
                                            <form method="post" action="$linksignup$" class="requestAccount">
                                              <input class="shiny float-left"
                                                     name="email" type="email"
                                                     infotext="$_FillInEmail()$"
                                                     autocomplete="off"
                                                     /> <a href="#" class="submit
                                                                           new-btn-small green2 center
                                                                           float-right"><p>$_ButtonCreateAccount()$!</p></a>
                                              <input type="submit" class="hidden" />
                                            </form>
                                          </div>
                                        </div>
                                        $endif$
        </div>
        </div>

        $pageFooter()$
      </body>
####
queryColumn=
<div class="white thin box createaccountbox">
                                                <h4>$_queryColumnHead()$</h4>
<br />
<p>$_queryColumnBody1()$</p>
<br />
<p>$_queryColumnBody2()$</p>
                                                <br />
                                                <form method="post"
                                                action="$linksignup$" class="requestAccount">
                                                        <input type="email"
                                                        name="email"
                                                        infotext="$_email()$"
                                                        autocomplete="off"
                                                        /><br /><br />

                                                        <a class="submit new-btn-small green2 center"><p>$_ButtonCreateAccount()$</p></a>
                                                        <input type="submit" class="hidden" />
                                                </form>
                                        </div>
                                        <br />
                                        <div class="white thin box">
                                                <h4>$_queryColumnQuestions()$</h4>
                                                <br />

                                                <p>$_queryColumnPhone()$</p>
                                                <p>$_queryColumnEmail()$</p>

                                                <br />
                                                <form method="post"
                                                      action="$linkquestion$">
                                                        <input type="text" name="name" infotext="$_name()$" autocomplete="off" /><br />
                                                        <input type="email" name="email" infotext="$_email()$" autocomplete="off"/><br />
                                                        <input type="tel" name="phone" infotext="$_phone()$" autocomplete="off"/><br />
                                                        <textarea name="message" infotext="$_message()$" style="height:42px;" autocomplete="off"></textarea>
                                                        <a class="submit new-btn-small green2 center"><p>$_send()$!</p></a>
                                                        <input type="submit" class="hidden" />
                                                </form>

                                        </div>
####
pageFooter=
<div id="footer-container" class="newdesign">
  <div class="top-border"></div>
  <div class="spacer"></div>
  <div class="content">
    <div class="col left">
      <strong>$_pageFooterTheService()$</strong>
      <ul>
        <!--<li><a href="/features.html">Funktioner</a></li> -->
        <!--<li><a href="/benefits.html">Fördelar</a></li> -->
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
<script type="text/javascript" src="//asset0.zendesk.com/external/zenbox/v2.1/zenbox.js"></script>
<style type="text/css" media="screen, projection">
  @import url(//asset0.zendesk.com/external/zenbox/v2.1/zenbox.css);
</style>
<script type="text/javascript">
safeReady(function() {
  if (typeof(Zenbox) !== "undefined") {
    Zenbox.init({
      dropboxID:   "20010532",
      url:         "https://skrivapa.zendesk.com",
      tabID:       "support",
      tabColor:    "black",
      tabPosition: "Right"
    });
  }
})
</script>

<div class="imgCache">
  <img src="/theme/../img/spr-flash-bg.png" />
  <img src="/img/spr-flash-bg.png" />
</div>
####
loginBox=
    <div id="login">
      <form action="/login" method="post">
        <input type="hidden" name="referer" value="$referer$" />
        <table>
	  <tr>
            <td>$_email()$:</td>
            <td><input type="email" name="email" autocomplete="off" class="noflash" value="$if(email)$$email$$endif$" /></td>
          </tr>
	  <tr>
            <td>$_password()$:</td>
            <td><input type="password" name="password" autocomplete="off" /></td>
          </tr>
	  <tr>
            <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in" /></td>
            <td><a href="/amnesia"> $_forgotpassword()$</a></td>
	  </tr>
        </table>
      </form>
    </div>

####
header=
 <div id="top-slim-container" class="newdesign">
                        <div class="content">
                                <a href="$linkhome$"
                                class="scrive-logo"></a>


                                $if(logged)$

                                <!--
                               <span id="inviteContainer">
                                 <form method="post" action="$linkinvite$" class="MultiFile-intercepted">
                                 <div>
                                  <div>
                                       <input type="email" class="float-left" autocomplete="off" name="invitedemail" infotext="$_headerInviteFriend()$"/>
                                       <a class="btn-tiny green float-right submit" href="#">
                                            <div class="left"></div>
                                            <div class="label"><p>$_headerInvite()$</p></div>
                                            <div class="right"></div>
                                       </a>
                                       <input type="submit" class="hidden" name="invite" value="Skicka" />
                                  </div>
                                 </div>
                                 </form>
                                </span>
                                 -->
                               <a href="$linklogout$" class="toplink logout">$_headerLogOut()$</a>
                               <a href="$linkaccount$" class="toplink">$_headerAccount()$</a>
                               <a href="$linkissue$" class="toplink">$_headerArchive()$</a>
                               <a href="$linkupload$" class="toplink">$_headerSend()$</a>
                                $localeSwitcher()$
                                $else$
                                <span id="moveleft2">
                                    <a href="#" class="login-button"><p>$_login()$</p></a>
                                    <a href="$linkcontactus$" class="toplink">$_pageHeaderContactUs()$</a>
                                    <a href="$linkabout$" class="toplink">$_pageFooterAbout()$</a>
                                    <a href="$linklegal$" class="toplink">$_pageFooterLaw()$</a>
                                    <a href="$linkpriceplan$" class="toplink">$_pageFooterPrices()$</a>
                                $localeSwitcher()$
                                  </span>

                                $endif$

                            </div>

            <div class="clearboth"></div>
          </div>
####
localeSwitcher=
  <div class="localeSwitcher">
    $if(islocaleswitch)$
    $if(!logged)$
    <div><a href="$linksesv$" class="icon flag swedish $if(localesweden)$active$endif$"></a></div>
    <div><a href="$linkgben$" class="icon flag british $if(localebritain)$active$endif$"></a></div>
    $else$
    <div><form action="$linklocaleswitch$" method="POST">
           <input type="hidden" name="region" value="REGION_SE"/>
           $if(linksesv)$
           <input type="hidden" name="referer" value="$linksesv$"/>
           $endif$
           <a href="" class="submit icon flag swedish $if(localesweden)$active$endif$"></a>
         </form></div>
    <div><form action="$linklocaleswitch$" method="POST">
           <input type="hidden" name="region" value="REGION_GB"/>
           $if(linkgben)$
           <input type="hidden" name="referer" value="$linkgben$"/>
           $endif$
           <a href="" class="submit icon flag british $if(localebritain)$active$endif$"></a>
         </form></div>
    $endif$
    $endif$
  </div>
####
