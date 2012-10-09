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
        $if(production)$
        <!-- Snoobi code tracking code -->
            <script type="text/javascript">
            var psSite = "8d72086524";
            var peJsHost = "https://";
            document.write(unescape("%3Cscript src='https://tr.prospecteye.com/track.js' type='text/javascript'%3E%3C/script%3E"));
            </script>
            <noscript><img src="https://tr.prospecteye.com/?id=8d72086524" style="border:0;display:none;"/></noscript>
       <!-- End of snoobi -->
       $endif$

###
