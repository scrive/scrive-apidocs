###
priceplanPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_priceplanPageTitle()$</title>
        <meta name="description" content="$_priceplanPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBody(unwrappedbody=priceplanPageContents();showCreateAccount=False)$
    </html>
###
priceplanPageContents=
<div class="js-price-plan price-plan-outside"></div>

<script>
  PricePage({}).show('.js-price-plan');
</script>
###
