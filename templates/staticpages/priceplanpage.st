###
priceplanPage=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    $if(logged)$
      <link href='/service_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $else$
      <link href='/scrive_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $endif$
  </head>
<body>
<div class="wrapper-position-footer">
  $header()$
  <div class="mainContainer" >
    <div class="body-container pricing">
      <div class="big-title"><h1>$_paymentsSubscribeHeader()$</h1></div>

      <div class="js-price-plan price-plan price-plan-outside"></div>

      <script>
      require(['jquery', 'legacy_code'], function(jQuery) {
        jQuery('.price-plan-page-link').addClass('active');
        PricePage({}).show('.js-price-plan');
        mixpanel.track('View Price Plan Page');
      });
      </script>

    </div>
  </div>
  $pageFooter()$
</div>
</body>
$htmlclosingtag()$
###
