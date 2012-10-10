###
contactUsPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_termsPageTitle()$</title>
        <meta name="description" content="$_contactUsPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="/libs/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      <body>
         $standardBodyWithQueryCol(unwrappedbody=contactUsPageContents())$
      </body>
    </html>
###
contactUsPageContents=
                                        <div class="opaque wide box" style="height:453px;">
                                          <iframe width="662" height="453" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="https://maps.google.com/maps/ms?$if(localesweden)$hl=sv$endif$$if(localebritain)$hl=en$endif$&amp;msa=0&amp;msid=205556323418288588249.0004ca37e0471835ffad9&amp;ie=UTF8&amp;t=m&amp;ll=59.335816,18.06324&amp;spn=0.079318,0.226936&amp;z=12&amp;output=embed"></iframe> </div>
                                        <br />
                                        <div class="opaque wide box smaller" style="min-height: 0px;">
                                        <div style="margin-left:auto;margin-right:auto;width:500px">
                                        <h3>   $_pageFooterContact1()$ </h3>
                                        <BR/>
                                        <table class="contactus" style="margin-left:auto;margin-right:auto;width:500px">
                                            <tbody>
                                                <tr>
                                                    <td style="width:200px;vertical-align:top">  $_pageFooterContact2()$ </td>
                                                    <td style="width:200px;vertical-align:top">  $_pageFooterContact3()$ </td>
                                                </tr>
                                                <tr>
                                                    <td style="width:200px;vertical-align:top">  <BR/></td>
                                                    <td style="width:200px;vertical-align:top">  <BR/></td>
                                                </tr>
                                                <tr>
                                                    <td style="width:200px;vertical-align:top">  $_pageFooterContact4()$ </td>
                                                    <td style="width:200px;vertical-align:top">  $_pageFooterContact5()$ </td>
                                                </tr>
                                            </tbody>
                                        </table>
                                        </div>
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
