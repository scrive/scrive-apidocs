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
<div class="nicetext" style="margin-top: 11px; margin-bottom: 11px;">
<h1>Scrive is hiring</h1>

<p style="margin-top: 11px; margin-bottom: 11px;">Scrive is growing and Scrive is hiring!</p>

<p style="margin-top: 11px; margin-bottom: 11px;">We are looking for a wise programmer that could help us provide best online signing service in the world.</p>

<p style="margin-top: 11px; margin-bottom: 11px;">We are a Swedish company that moves signatures from paper to online. Founded two years ago we are constantly growing and right now we approach a new phase of development. We plan to expand our operations in Sweden and in Poland, so you should be either living in Sweden or speaking Polish (or both). </p>

<p style="margin-top: 11px; margin-bottom: 11px;">Technologies you should be comfortable with:
<ul style="padding-left: 2em; list-style: disc;">
<li>Haskell (of course!)</li>
<li>PostgreSQL, SQL</li>
<li>HTML, JavaScript, jQuery, backbone.js</li>
<li>Linux, git</li>
</ul>
</p>

<p style="margin-top: 11px; margin-bottom: 11px;">Employment conditions:
<ul style="padding-left: 2em; list-style: disc;">
<li>full time</li>
<li>flexible work hours</li>
<li>work online or at the Scrive Stockholm office</li>
</ul>
</p>

<p style="margin-top: 11px; margin-bottom: 11px;">Interested? Contact me at: gracjan!!scrive.com<br/>
Gracjan Polak, Scrive CTO</p>
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
