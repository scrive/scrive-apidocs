###
priceplanPage=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    $if(logged)$
      <link href='$cdnbaseurl$/service_branding/$brandingdomainid$/$if(brandinguserid)$$brandinguserid$$else$_$endif$/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $else$
      <link href='$cdnbaseurl$/scrive_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $endif$
  </head>
<body>
<div class="wrapper-position-footer">
  $header()$
  <div class="mainContainer" >
    <div class="body-container pricing">
      <div class="big-title"><h1>$_paymentsSubscribeHeader()$</h1></div>
      <div class="js-price-plan price-plan price-plan-outside"></div>
      <script src="/compiled/all/pricepage.js"></script>
    </div>
  </div>
  <div class="pricing-footer">
    $pageFooter()$
  </div>
</div>
</body>
$htmlclosingtag()$
###
