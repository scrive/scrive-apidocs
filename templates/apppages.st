###
pageForgotPassword=
    <div class="centerdivnarrow">
      <form action=$LinkForgotPassword()$ method="post">
        <table>
          <tr>
            <td>E-mail</td>
            <td><input name="email" type="email"></td>
          </tr>
        </table>
        <input type="submit" value="Skicka nytt lösenord" class="validateEmail">
      </form>
    </div>

###
pageForgotPasswordConfirm=
    <div class="centerdivnarrow">
      <p>Ett nytt lösenord har skickats till din e-post. Du kan nu logga in med dina nya uppgifter.</p>
      $loginBox$
    </div>

###
pageLogin= 
    <div class="centerdivnarrow">
      <p class="headline">Logga in SkrivaPå!</p> 
      $loginBox()$
    </div>

###
wholePage=
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
    <html>
      <head>
        <title>$title$$if(production)$$else$ (devel) $endif$</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/style.css" media="screen">
        <link rel="stylesheet" type="text/css" href="$prefix$/theme/calendar.css" media="screen">
        <link rel="stylesheet" type="text/css" 
              href="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/ui-lightness/jquery-ui.css"
              media="screen">
        
        <script src="$protocol$//ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.js"
                type="text/javascript"></script>
        <script src="$protocol$//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/jquery-ui.min.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/jquery.tools.min.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/jquery.MultiFile.js"
                type="text/javascript"></script>
        <script src="$prefix$/tiny_mce/jquery.tinymce.js"
                type="text/javascript"></script>
        <script src="$prefix$/js/global.js"
                type="text/javascript"></script>
      </head>
      <body>
        <div id="headerWide"></div>
        <div id="mainContainer960">
          <div class="flashmsgbox">
            $flashmessages:{ msg | <div class="flashmessage">$msg:noescape()$</div> }$
          </div>
   
          <div id="headerContainer">
            <a href="/">
              $if(production)$
              <img id="logosmall" src="/theme/images/logosmall.png" alt="Liten logga">
              $else$
              <span id="logosmall">Staging area</span>
              $endif$
            </a> 
            $if(userfullname)$
            <span id="inviteContainer">
              <form action="/invite" method="post">
                <div>
                  <div>Ge bort ett premiumkonto!</div>
                  <div><input type="email" infotext="Fyll i mottagarens e-post" name="invitedemail" autocomplete="off"/>
                       <input type="submit" value="Skicka" name="invite" class="button"/></div>
                </div>
              </form>
            </span>
            <span id="userMenu">$userfullname$ | <a href="/account">Konto</a> | <a href="/logout">Logga ut</a></span>
            $else$
            <div id="loginContainer">
	      <form action="/login" method="post"> 
	        <div> 
	          <input type="email" infotext="Användarnamn" name="email" autocomplete="off"> 
	          <input type="password" name="password" infotext="password" autocomplete="off"><br> 
	          <a href="/amnesia"> Glömt lösenord</a> 
                  <input type="submit" value="Logga in" name="login" class="button"> 
	        </div> 
	      </form> 
            </div> 
            $endif$     
           <div id="nav">
           $if(uploadTab + documentTab)$
                 <ul>
               $if(uploadTab)$
                  <li>  <a href="$uploadTab.0$" $if(uploadTab.1)$ class="active" $endif$>Skapa avtal</a></li>
               $endif$
               $if(documentTab)$
                   <li>  <a href="$documentTab.0$" $if(documentTab.1)$ class="active" $endif$>Arkiv</a></li>
               $endif$ 
                 </ul>
           $endif$
            </div>
            <div class="clearboth"></div>
          </div>
          $content:noescape()$
        </div>
      
      
        <div id="footerContainer">
          <div id="footerContainer2">
            <ul>
              <li class="footerCategoryHeader"> 
                SkrivaPå
              </li>
              <li>
                <a href="/why.html">Fördelar</a>
              </li>
              <li>
                <a href="/features.html">Funktioner</a>
              </li>
              <li>
                <a href="/pricing.html">Prisplan</a>
              </li>
            </ul>

            <ul>
              <li class="footerCategoryHeader"> 
                Trygghet och villkor
              </li>
              <li>
                <a href="/security.html">Säkerhet</a>
              </li>
              <li>
                <a href="/legal.html">Juridik</a>
              </li>
              <li>
                <a href="/privacypolicy.html">Sekretesspolicy</a>
              </li>
              <li>
                <a href="/termsofuse.html">Allmäna Villkor</a>
              </li>
            </ul>

            <ul>  
              <li class="footerCategoryHeader"> 
                Om oss
              </li>
              <li>
                <a href="/contact.html">Kontakt</a>
              </li>
            </ul>
		
	    <div id="copy">&copy; 2010 SkrivaPå</div> 
          </div>
        </div>
        $if(production)$
        <script type="text/javascript">
          var _gaq = _gaq || [];
          _gaq.push(['_setAccount', 'UA-6387711-9']);
          _gaq.push(['_trackPageview']);

          (function() {
          var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
          })();

        </script>
        <script type="text/javascript" src="https://eu1.snoobi.com/snoop.php?tili=skrivapa_se"></script>
        $endif$
      </body>
    </html>

####
loginBox=
    <div id="login">
      <form action="/login" method="post">
        <input type="hidden" name="referer" value="$referer$">
        <table>
	  <tr>
            <td>E-mail:</td> 
            <td><input type="email" name="email" autocomplete="off" class="noflash" value="$if(email)$$email$$endif$"></td> 
          </tr>
	  <tr> 
            <td>Lösenord:</td> 
            <td><input type="password" name="password" autocomplete="off"></td> 
          </tr>
	  <tr> 
            <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"></td>
            <td><a href="/amnesia"> Glömt lösenord</a></td>
	  </tr>
        </table>
      </form>
    </div>

