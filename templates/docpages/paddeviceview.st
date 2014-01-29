####
padQueueCurrentPage=
$doctype()$
$htmlopentag()$
  <head>
    <title>Pad signing</title>
    $standardHeaderContents()$
    <link rel="apple-touch-startup-image" href="/img/logo-scrive-big-black.png"/>
    <meta name="apple-mobile-web-app-capable" content="yes" />
   <meta name="viewport" content="maximum-scale=1.2, minimum-scale=0.6" />
    <link rel="apple-touch-icon" href="/img/ipad-logo.png"/>
  </head>
  <body class="signview" style="min-height:100%">
    $flashmessages()$
    <div class="wrapper-position-footer">
        <div class="mainContainer" >
          <div class="body-container">
            <div class="padview"></div>
            <script>
             require(['jquery', 'legacy_code'], function(jQuery) {
            jQuery(function(){
                var pad = new KontraPad();
                jQuery(".padview").append(pad.view().el)
            });
             });
            </script>
          </div>
        </div>
    </div>
  </body>
$htmlclosingtag()$
#####
