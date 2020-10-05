####
pagePrivilegesConfirm =$doctype()$
  $htmlopentag()$
      <head>
        <title>$pagetitle()$</title>
        $IECompatibilityMetaTag()$
        $standardHeaderContents(nobranding='true')$
        <link href='$cdnbaseurl$/service_branding/$brandingdomainid$/$if(brandinguserid)$$brandinguserid$$else$_$endif$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
       </head>
       <body>
        <header class="site">
          <nav >
            <ul class="ct">
              <li id="branding" >
                <a id="logo" class="page" href="#" >
                  <img src="$cdnbaseurl$/service_logo/$brandingdomainid$/$if(brandinguserid)$$brandinguserid$$else$_$endif$/$brandinghash$" style="margin:0px;" />
                </a>
              </li>
              <ul class="right-container" >
                <li class="float-right" >
                  <a class="page js-deny" href="#" >
                    $_apiConfiramtionDeny()$
                  </a>
                </li>
              </ul>
            </ul>
          </nav>
        </header>
        $flashmessages()$
        <div class='oauth-confirm'></div>
        <script>
        var fromTemplate = {
          logged : $if(logged)$ true $else$ false $endif$,
          sendPermission : $if(isDocumentSend)$ true $else$ false $endif$,
          createPermission : $if(isDocumentCreate)$ true $else$ false $endif$,
          readPermission : $if(isDocumentCheck)$ true $else$ false $endif$,
          fullAccessPermission : $if(isFullAccess)$ true $else$ false $endif$,
          who : "$companyname$",
          token : "$token$"
        };
        </script>
        <script src="$cdnbaseurl$/compiled/all/oauth_accept-$versioncode$.js" crossorigin="anonymous"></script>
      </body>
    $htmlclosingtag()$
#####
