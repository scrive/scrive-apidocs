####
padQueueCurrentPage=
$doctype()$
<html>
  <head>
    <title>Pad signing</title>
    $standardHeaderContents()$
    <link rel="apple-touch-startup-image" href="/img/logo-scrive-big-black.png"/>
    <meta name="apple-mobile-web-app-capable" content="yes" />
    <link rel="apple-touch-icon" href="/img/ipad-logo.jpg"/>
  </head>
  <body class="signview" style="min-height:100%">
    <div class="mainContainer960" >
      <div class="body-container">
        $padQueueCurrentPageContent()$
      </div>
    </div>
  </body>
</html>
#####
padQueueCurrentPageContent=
<div class="padview"></div>
<script>
jQuery(function(){
    var padTable = KontraPad.init({ });
    jQuery(".padview").append(padTable.view.el)
});

</script>
######