###
priceplanPage=
<div class="mainContainer" >
  <div class="body-container">

    <div class="js-price-plan price-plan"></div>
    <script>
      jQuery('body').css('background','none repeat scroll 0 0 #FFFFFF');
      jQuery('.price-plan-page-link').addClass('active');
      PricePage({}).show('.js-price-plan');
      mixpanel.track('View Price Plan Page');
    </script>

 </div>
</div>
###
priceplanPageWithBranding=
$doctype()$
<html>
  <head>
    <script src="//cdn.optimizely.com/js/158965942.js"></script>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$">
    <div style='width:275px;margin:20px auto;'>
      <div style="margin-bottom: 63px">
        <img alt="logo" src="$logolink$"/>
        <div class="divider-line"></div>
        <label style="text-align:center;width:275px;">E-signing powered by Scrive</label>
      </div>
    </div>
    <div class="js-price-plan price-plan"></div>
    <script>
      PricePage({noheaders: true}).show('.js-price-plan');
      mixpanel.track('View Price Plan Page | Branded');
    </script>

  </body>
</html>
####