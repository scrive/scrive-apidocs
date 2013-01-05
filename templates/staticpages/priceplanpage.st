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
