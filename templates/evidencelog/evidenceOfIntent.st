#####
evidenceOfIntent=
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Evidence Of Intent Log for "$documenttitle$"</title>
  </head>
  <body>
    <h1></h1>

    <div style="font-family:'Lucida Grande', sans-serif;margin-top:0px;padding:0px;">

      <table width="100%" cellpadding="0" cellspacing="0" style="margin: auto;"><tr><td>
        <table id="main" cellpadding="0" cellspacing="15" width="960" align="center" style="margin: auto;background-color:#ffffff;border:1px solid #DDDDDD;">
          <tr>
            <td style="padding:10px; margin: auto;">
              <table id="header" width="580" cellpadding="9" 
                     bgcolor="212121" cellspacing="0"
                     align="center"
                     style="color:#ffffff;text-align:center;margin:auto">
                <tr>
                  <td style="text-align:left; margin: auto;padding:10px;">
                  </td>
                </tr>
                <tr>
                  <td>
                    <table id="banner" width="800" cellpadding="0" bgcolor="212121" cellspacing="0" align="center" style="color:#fff;text-align:center;">
                      <tr>
                        <td width="800" cellpadding="0">
                          <span style="font-weight:100;font-size:32px;margin:0px;letter-spacing:0.04em;color:#fff;font-family:'Copse',
                                       serif;padding:10px">
                            $documenttitle$
                          </span>
                        </td>
                      </tr>
                      <tr>
                        <td width="580">
                          <span style="display:block;font-weight:400;font-family:'Lucida Grande',sans-serif;font-size:18px;margin:0px;margin-top:12px;margin-bottom:20px;color:#fff;">
                            Evidence Of Intent Log
                          </span>
                        </td>
                      </tr>
                    </table>
                  </td>
                </tr>
              </table>
            </td>
          </tr><!-- header -->
          <tr>
            <td>
              <table id="content" width="800" cellspacing="0"
                     align="top" style="margin:auto;color:#212121;font-family:'Lucida Grande',sans-serif;font-size:12px;">
                $entries:{entry|
                <tr>
                  <td>
                    <table cellpadding="0" cellspacing="5" align="center" style="margin: auto;background-color:#ffffff;border:1px solid #DDDDDD;">
                      <tr>
                        <td>
                          $if(entry.first.time)$
                          <table cellpadding="0" cellspacing="5" align="center" style="margin: auto;background-color:#ffffff;border:1px solid #DDDDDD;">
                            <tr>
                              <td>At time $entry.first.time$ (as reported by client), $entry.signatory$ opened the sign view:
                              <br/><img alt="first" src="$entry.first.image$"/>
                              </td>
                            </tr>
                            </table>$endif$
                            $if(entry.signing.time)$
                            <table cellpadding="0" cellspacing="5" align="center" style="margin: auto;background-color:#ffffff;border:1px solid #DDDDDD;">
                              <tr>
                                <td>At time $entry.signing.time$ (as reported by client), $entry.signatory$ signed the document:
                                <br/><img alt="signing" src="$entry.signing.image$"/>
                                </td>
                              </tr>
                              </table>$endif$
                        </td>
                      </tr>
                    </table>
                  </td>
                  </tr>}$
              </table>
            </td>
          </tr>
      </table></td></tr></table>
    </div>
  </body>
</html>
###
