###
jobsPage=
    $doctype()$
    <html>
      <head>
        <title>Scrive is hiring</title>
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      <body>
         $standardBodyWithQueryCol(unwrappedbody=jobsPageContents())$
      </body>
    </html>
###
jobsPageContents=
<div class="nicetext">
<h1>Scrive is hiring</h1>

</div>
###
