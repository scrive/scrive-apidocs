######
nicemail=
<BR/>
<div style="font-family:'Lucida Grande', sans-serif;margin-top:0px;padding:0px;">

<table width="100%" cellpadding="0" cellspacing="0" style="margin: auto;"><tr><td>
    <!--
    <table id="top-message" cellpadding="0" cellspacing="0" width="600" align="center" style="font-family:sans-serif;font-weight:bold;color:#aaaaaa;font-size:10px;">
        <tr>
            <td align="center" height="30">
                $if(mailtop)$ $mailtom$ $endif$ <p>Kan du inte läsa detta brev? <a href="#">Öppna det i din webbläsare här.</a></p>
            </td>
        </tr>
    </table>
    -->
    <table id="main" cellpadding="0" cellspacing="15" width="630" align="center" style="margin: auto;background-color:#ffffff;border:1px solid #DDDDDD;">
        <tr>
            <td style="padding:10px; margin: auto;">
                <table id="header" width="580" cellpadding="9" bgcolor="$nicemailbgcolour()$"
                    cellspacing="0" align="center" style="color:$nicemailfgcolour()$;text-align:center;margin:auto">
                    <tr>
                        <td style="text-align:center; margin: auto;padding:20px;min-height:33px;">
                          <img src="$nicemaillogo()$" style="margin:auto;"></img>
                        </td>
                    </tr>
                    <tr>
                    <td>
                        <table id="banner" width="590" cellpadding="0" bgcolor="$nicemailbgcolour()$">
                            <tr>
                                <td width="580" align="center" cellpadding="0" style="text-align:center;"><span style="font-weight:100;font-size:24px;margin:0px;letter-spacing:0.04em;color:$nicemailfgcolour()$;font-family:'Copse', serif;padding:10px">
                                    $if(mailheader)$ $mailheader$ $endif$
                                    </span>
                                </td>
                            </tr>
                            <tr>
                                <td width="580" height="10"><hr style="height:1px;border:none;background:$nicemailfgcolour()$;width:330px;" /></td>
                            </tr>
                            <tr>

                                <td width="580" align="center" style="text-align:center;"><span style="display:block;font-weight:100;font-family:'Lucida Grande',sans-serif;font-size:11px;margin:0px;margin-bottom:20px;color:$nicemailfgcolour()$;">
                                       $if(!service)$ $_mailsubheader()$ $endif$
                                    </span>
                                </td>
                            </tr>
                        </table>
                        </td>
                    </tr>
                </table><!-- header -->
            </td>
        </tr><!-- header -->
        <tr>
            <td>
                <table id="content" width="590" cellspacing="0" align="center" style="margin: auto;color:#212121;font-family:'Lucida Grande', sans-serif;font-size:12px;">
                    <tr>
                        <td style="padding-left:25px; padding-right: 25px; padding-top: 5px; padding-bottom: 5px; margin: auto;"><img src="$ctxhostpart$/img/email-line.jpg" alt="" width="540" height="10" /></td>
                    </tr>
                    $if(mailp1)$ $nicemailparagraph(p=mailp1)$ $endif$
                    $if(mailp2)$ $nicemailparagraph(p=mailp2)$ $endif$
                    $if(mailp3)$ $nicemailparagraph(p=mailp3)$ $endif$
                    $if(mailp4)$ $nicemailparagraph(p=mailp4)$ $endif$
                    $if(mailp5)$ $nicemailparagraph(p=mailp5)$ $endif$
                </table>
            </td>
        </tr>
    </table><!-- main -->

    <table id="bottom-message" cellpadding="20" cellspacing="0" width="600" align="center">
    </table><!-- bottom message -->

</td></tr></table><!-- wrapper -->

</div>
#####
nicemailbgcolour=$if(!service.barsbackground)$$if(!companybrand.barsbackground)$212121$else$$companybrand.barsbackground$$endif$$else$$service.barsbackground$$endif$
#####
nicemailfgcolour=$if(!companybrand.barstextcolour)$#ffffff$else$$companybrand.barstextcolour$$endif$
#####
nicemaillogo=$if(!service.logo)$$if(!companybrand.logo)$$ctxhostpart$/img/email-logo.png$else$$ctxhostpart$$companybrand.logoLink$$endif$$else$$ctxhostpart$$service.logoLink$$endif$
#####
nicemailparagraph =
      <tr>
        <td style="padding-left:25px; padding-right:25px; padding-top: 5px; padding-bottom: 5px; margin: auto;">$p:noescape()$</td>
      </tr>
      <tr>
         <td style="padding-left:25px; padding-right:25px; padding-top: 5px; padding-bottom: 5px; margin: auto;"><img src="$ctxhostpart$/img/email-line.jpg" alt="" width="540" height="10" /></td>
      </tr>
####