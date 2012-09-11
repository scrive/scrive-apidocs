####
pagePrivilegesConfirm =
  $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
       <body>
        $flashmessages()$
        <div class='oauth-confirm' ></div>
        <script>
          jQuery(function() {
            var target = jQuery(".oauth-confirm");
            var confirmation =  new OAuthConfiration({
                               logged : $if(logged)$ true $else$ false $endif$,
                               sendPermission : $if(isDocumentSend)$ true $else$ false $endif$,
                               createPermission : $if(isDocumentCreate)$ true $else$ false $endif$,
                               readPermission : $if(isDocumentCheck)$ true $else$ false $endif$,
                               who : "$companyname$",
                               token : "$token$"
                        }).view().el
            target.append(confirmation);
          });
        </script>
      </body>
    </html>
#####