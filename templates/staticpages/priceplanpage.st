###
priceplanPage=
<div class="mainContainer" >
  <div class="body-container pricing">

    <div class="js-price-plan price-plan price-plan-outside"></div>
    <script>
     require(['jquery', 'legacy_code'], function(jQuery) {
      jQuery('body').css('background','none repeat scroll 0 0 #FFFFFF');
      jQuery('.price-plan-page-link').addClass('active');
      PricePage({}).show('.js-price-plan');
      mixpanel.track('View Price Plan Page');
     });
    </script>

 </div>
</div>
###
priceplanPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$">
    <div style='width:920px;margin: 0px auto;position: relative;top:170px; left: 185px;'>
      <label style='float:left;$if(externaltextcolour)$color:$externaltextcolour$;$endif$' class='label-with-link'>$_loginOrSignup()$</label>
    </div>
    <div style='width:275px;margin:20px auto;'>
      <div style="margin-bottom: 100px; margin-top: 50px;">
        <img alt="logo" src="/branding/logo"/>
        <div class="divider-line"></div>
        <label style="text-align:center;width:275px;$if(externaltextcolour)$color:$externaltextcolour$;$endif$">E-signing powered by Scrive</label>
      </div>
    </div>
    <div class="js-price-plan price-plan price-plan-branded"></div>
    <script>
     require(['jquery', 'legacy_code'], function(jQuery) {
      PricePage({noheaders: true,
                 includeenterprise: false,
                 buttoncolorclass : "$buttoncolorclass$",
                 headercolour: "$headercolour$",
                 textcolour: "$textcolour$",
                 pricecolour: "$pricecolour$",
                 externaltextcolour : "$externaltextcolour$",
                 linkcolour :  "$linkcolour$"
                 }).show('.js-price-plan');
      mixpanel.track('View Price Plan Page | Branded');
     });
    </script>

  </body>
$htmlclosingtag()$
####
