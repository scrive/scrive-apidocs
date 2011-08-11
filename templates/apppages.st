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
        <meta http-equiv="content-language" content="sv" />
        <link href='$protocol$//fonts.googleapis.com/css?family=Copse' rel="stylesheet" type="text/css"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/newdesign.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/style.css" media="screen"/>
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/calendar.css" media="screen"/>
		<link rel="stylesheet" type="text/css" href="$prefix$/theme/symbols.css" media="screen"/>
		<link rel="stylesheet" type="text/css" href="$prefix$/theme/buttons.css" media="screen"/>
        <link href="$prefix$/theme/flash.css" rel="stylesheet" type="text/css" media="screen" />
        <link href="/css/main.css" rel="stylesheet" type="text/css" />
        <link rel="stylesheet" type="text/css"  href="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.11/themes/ui-lightness/jquery-ui.css"              media="screen" />
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

        <script src="$prefix$/js/infotextinputs.js"
                type="text/javascript"></script>
        
        <script src="$prefix$/js/buttons.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/uploadbuttons.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/tooltips.js"
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
        <script src="$prefix$/js/documents.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/docview.js"
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
        <div id="body-container">
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
                                                                           float-right"><p>$_createaccount()$!</p></a>
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
<div class="white thin box">
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

                                                        <a class="submit new-btn-small green2 center"><p>$_createaccount()$</p></a>
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
        <li><a href="/priser">$_pageFooterPrices()$</a></li>
      </ul>
      <br />
      <strong>$_pageFooterTerms()$</strong>
      <ul>
        <li><a href="/sakerhet">$_pageFooterSecurity()$</a></li>
        <li><a href="/juridik">$_pageFooterLaw()$</a></li>
        <li><a href="/sekretesspolicy">$_pageFooterPrivacy()$</a></li>
        <li><a href="/allmana-villkor">$_pageFooterTOS()$</a></li>
      </ul>
      <br />
      <strong style='text-transform:uppercase'>$ourServiceName()$</strong>
      <ul>
        <li><a href="/">$_pageFooterHome()$</a></li>
        <li><a href="/om-skrivapa">$_pageFooterAbout()$</a></li>
        <li><a href="http://support.skrivapa.se">$_pageFooterSupportFeedback()$</a></li>
        <li><a href="/partners">$_pageFooterPartners()$</a></li>
        <li><a href="/kunder">$_pageFooterCustomers()$</a></li>
        <!--<li><a href="/board.html">Board of Advisors</a></li>-->
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
                                <a href="/" class="$ourLogoClass()$"></a>
                                $if(logged)$
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
                                 
     <a href="$linklogout$" class="document-button">$_headerLogOut()$</a>
     <a href="$linkaccount$" class="document-button">$_headerAccount()$</a>
     <a href="$linkissue$" class="document-button">$_headerArchive()$</a>
     <a href="$linkmain$" class="document-button">$_headerSend()$</a>

                                $else$
                                <a href="#" class="login-button"><p>$_login()$</p></a>
                                $endif$
                            </div>
                           
            <div class="clearboth"></div>
          </div>
####
