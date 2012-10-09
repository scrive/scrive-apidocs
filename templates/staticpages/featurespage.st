###
featuresPage=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$ Scrive Features</title>
    <meta name="description" content="$_featuresPageMetaDescription()$" />
    $standardHeaderContents()$
    $if(production)$
    <script src="/libs/analytics.js" type="text/javascript"></script>
    $endif$
  </head>
  $standardBody(unwrappedbody=featuresPageContents())$
</html>
###
featuresPageContents=
  <div class="big-pp-container">
    <table>
      <thead>
        <tr>
          $_priceplanPagePriceplanTop()$
        </tr>
      </thead>
      <tbody>
        <tr class="odd-row">$_priceplanPageTableRow001()$
        </tr>
        <tr>$_priceplanPageTableRow002()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow003()$
        </tr>
        <tr>$_priceplanPageTableRow004()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow005()$
        </tr>
        <tr>$_priceplanPageTableRow006()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow007()$
        </tr>
        <tr>$_priceplanPageTableRow008()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow009()$
        </tr>
        <tr>$_priceplanPageTableRow010()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow011()$
        </tr>
        <tr>$_priceplanPageTableRow012()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow013()$
        </tr>
        <tr>$_priceplanPageTableRow014()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow015()$
        </tr>
        <tr>$_priceplanPageTableRow016()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow017()$
        </tr>
        <tr>$_priceplanPageTableRow018()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow019()$
        </tr>
        <tr>$_priceplanPageTableRow020()$
        </tr>
        <tr class="odd-row">$_priceplanPageTableRow021()$
        </tr>
      </tbody>
    </table>
  </div>
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
