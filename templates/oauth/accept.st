####
pagePrivilegesConfirm =
  $doctype()$
  $htmlopentag()$
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents(nobranding='true')$
        <link href='/service_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
       </head>
       <body>
        <header class="site">
          <nav >
            <ul class="ct">
              <li id="branding" >
                <a id="logo" class="page" href="#" >
                  <img src="/service_logo/$brandinghash$" style="margin:0px;" />
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
         require(['jquery', 'legacy_code'], function(jQuery) {
          jQuery(function() {
            var target = jQuery(".oauth-confirm");
            var confirmation =  new OAuthConfirmation({
                               logged : $if(logged)$ true $else$ false $endif$,
                               sendPermission : $if(isDocumentSend)$ true $else$ false $endif$,
                               createPermission : $if(isDocumentCreate)$ true $else$ false $endif$,
                               readPermission : $if(isDocumentCheck)$ true $else$ false $endif$,
                               who : "$companyname$",
                               token : "$token$"
                        });
            target.append(confirmation.view().el);

            jQuery(".js-deny").click(function() {
               confirmation.deny();
               return false;
            })

          });
         });
        </script>
      </body>
    $htmlclosingtag()$
#####
