###
priceplanPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_priceplanPageTitle()$</title>
        <meta name="description" content="$_priceplanPageMetaDescription()$" />
        $standardHeaderContents()$
      </head>
      $standardBody(unwrappedbody=priceplanPageContents();showCreateAccount=False)$
    </html>
###
priceplanPageContents=
<div class="js-price-plan price-plan"></div>

<script>
  PricePage({}).show('.js-price-plan');
</script>
###
