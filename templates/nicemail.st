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
    <table id="main" cellpadding="0" cellspacing="15" width="600" align="center" style="margin: auto;background-color:#ffffff;border:1px solid #DDDDDD;">
        <tr>
            <td style="padding:10px; margin: auto;">
                <table id="header" width="580" cellpadding="9" bgcolor="212121" cellspacing="0" align="center" style="color:#ffffff;text-align:center;margin:auto;">
                    <tr>
                        <td style="text-align:left; margin: auto;padding:10px;"><img src="$ctxhostpart$/img/email-logo.png" width="70" height="22" /></td>
                    </tr>
                    <tr>
                    <td>
                        <table id="banner" width="560" cellpadding="0" bgcolor="212121" cellspacing="0" align="center" style="color:#fff;text-align:center;">
                            <tr>
                                <td width="580" cellpadding="0"><span style="font-weight:100;font-size:32px;margin:0px;letter-spacing:0.04em;color:#fff;font-family:'Copse', serif;">
                                    $if(mailheader)$ $mailheader$ $endif$
                                    </span>
                                </td>
                            </tr>
                            <tr>
                                <td width="580" height="10"><hr style="height:1px;border:none;background:#fff;width:330px;" /></td>
                            </tr>
                            <tr>
                                <td width="580"><span style="display:block;font-weight:100;font-family:'Lucida Grande',sans-serif;font-size:11px;margin:0px;margin-bottom:20px;color:#fff;">
                                    $if(mailsubheader)$ $mailsubheader$ $endif$
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
                <table id="content" width="560" cellspacing="0" align="center" style="margin: auto;color:#212121;font-family:'Lucida Grande', sans-serif;font-size:12px;">
                    <tr>
                        <td style="padding:5px; margin: auto;"><img src="$ctxhostpart$/img/email-line.jpg" alt="" width="540" height="10" /></td>
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
nicemailparagraph = 
      <tr>
        <td style="padding:5px; margin: auto;">$p:noescape()$</td>
      </tr>
      <tr>
         <td style="padding:5px; margin: auto;"><img src="$ctxhostpart$/img/email-line.jpg" alt="" width="540" height="10" /></td>
      </tr>
####