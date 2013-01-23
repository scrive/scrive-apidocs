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
            <td><div style="margin-left:100px;margin-right:100px">The
            following screen shots show the signing environments of
            the respective signing parties.  Each time stamp reflects
            the clock of the client, and may be different from the
            server clock.

            <p/>At any point in time, the signing service maintains a
            screen shot of a <em>reference sign view</em> showing a
            standard document at the moment it is being signed.  The
            reference sign view shot illustrates what the signing
            environment is supposed to look like for clients at the
            moment of signing, which is helpful for cases when
            clients' signing environments were unable to provide
            accurate screen shots.</div></td>
          </tr>
          <tr><td>
              <table id="content" width="800" cellspacing="0"
                     align="top" style="margin:auto;color:#212121;font-family:'Lucida Grande',sans-serif;font-size:12px;">
                <tr style="font-weight:800;font-family:'Lucida Grande',sans-serif;font-size:14px;margin:0px;">
                  <td style="border-bottom: solid 1px black;margin-bottom:60px;width:140px;">Time</td>
                  <td style="border-bottom: solid 1px black;margin-bottom:60px;width:80px;">IP</td>
                  <td style="border-bottom: solid 1px black;margin-bottom:60px;width:574px">Event</td>
                </tr>
                $entries:{entry|
                $if(entry.first.time)$
                <tr>
                  <td style="font-size:10px;padding-top:18px;">$entry.first.time$</td>
                  <td style="font-size:10px;padding-top:18px">$entry.ip$</td>
                  <td style="padding-top:18px;width:574px;max-width:574px;">
                    <a href="$entry.first.image$">$entry.signatory$ opened the sign view</a>.
                  </td>
                </tr>
                $endif$
                <tr>
                  <td style="font-size:10px;padding-top:18px;">$entry.signing.time$</td>
                  <td style="font-size:10px;padding-top:18px">$entry.ip$</td>
                  <td style="padding-top:18px;width:574px;max-width:574px;">
                    $if(entry.signing.image)$<a href="$entry.signing.image$">$entry.signatory$ signed the document</a>.
                    $else$                                                   $entry.signatory$ signed the document.
                    $endif$
                    Corresponding <a href="$entry.reference.image$">reference sign view</a>, created $entry.reference.time$.
                  </td>
                </tr>}$
              </table>
            </td>
          </tr>
      </table>
    </div>
  </body>
</html>
###
