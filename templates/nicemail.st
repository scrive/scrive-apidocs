nicemail=
<center class="rendered-email">
  <table border="0" cellpadding="0" cellspacing="0" width="100%" id="backgroundTable" style="margin: 0; padding: 0; width: 100%; vertical-align: top; line-height: 0px;background-color: $nicemailheaderbackground()$;">
    <tr>
      <td align="center" valign="top">
        <!-- // Begin Template Header \\ -->
        <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateHeader" style="background-color: $nicemailheaderbackground()$; border-bottom: 0;">
          <tr>
            <td class="headerContent" style="background-color: $nicemailheaderbackground()$; font-family: Helvetica Neue, Arial, sans-serif; font-size: 40px; font-weight: bold; line-height: 100%; padding: 0; text-align: left; vertical-align: middle" >
              <!-- // Begin Module: Standard Header Image \\ -->
              <table cellpadding="0" cellspacing="0">
                <tr>
                  <td style="padding-top: 32px; padding-bottom: 32px;">
                    <img src="$nicemaillogo()$" style="border: none; font-size: 14px; font-weight: bold; height: auto; line-height: 100%; outline: none; text-decoration: none; text-transform: capitalize; max-width:600px;" id="headerImage campaign-icon" />
                  </td>
                </tr>
              </table>
              <!-- // End Module: Standard Header Image \\ -->
            </td>
          </tr>
        </table><!-- // End Template Header \\ -->
      </td>
    </tr>

    <tr>
      <td align="center" valign="top" style="padding-bottom: 30px;">
        <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateContainer" style="background-color: $nicemailbgcolour()$; border: 1px solid $nicemailbordercolor()$;">
          <tr>
            <td></td>
          </tr>

          <tr>
            <td align="center" valign="top">
              <!-- // Begin Template Body \\ -->

              <table border="0" cellpadding="0" cellspacing="32" width="600" id="templateBody" style="border-collapse: separate;">
                <tr>
                  <td valign="top" class="bodyContent" style="background-color: $nicemailbgcolour()$">
                    <!-- // Begin Module: Standard Content \\ -->

                    <table border="0" cellspacing="0" width="100%">
                      <tr>
                        <td valign="top" style="color: $nicemailfgcolour()$; font-family: $nicemailfont()$; font-size: 16px; line-height: 150%; text-align: left">
                          <div>
                            $if(previewLink)$
                              <table>
                                <tr>
                                  <td valign="top" style="vertical-align: top;">
                                    <table>
                                      <tr>
                                        <td style="height: 106px; color: $nicemailfgcolour()$; font-family: Helvetica Neue, Arial, sans-serif; font-size: 16px; line-height: 0; text-align: left; border:1px solid #dee4ed; width: 150px;">
                                          <img src="$ctxhostpart$$previewLink$" style="border: none; font-size: 14px; font-weight: bold; height: auto; line-height: 100%; outline: none; text-decoration: none; text-transform: capitalize; display: inline; max-width:150px;"/>
                                        </td>
                                      </tr>
                                      <tr>
                                        <td style="height: 0px;">
                                        </td>
                                      </tr>
                                    </table>
                                  </td>
                                  <td style="width: 32px;">
                                  </td>
                                  <td style="float: right; width: 330px; color: $nicemailfgcolour()$; font-family: $nicemailfont()$; font-size: 16px; line-height: 150%; text-align: left">
                                    $nicemailparagraph(p=mailp1;l=mailp2;spacing=mailspacing1)$
                                    $nicemailparagraph(p=mailp2;l=mailp3;spacing=mailspacing2)$
                                    $nicemailparagraph(p=mailp3;l=mailp4;spacing=mailspacing3)$
                                    $nicemailparagraph(p=mailp4;l=mailp5;spacing=mailspacing4)$
                                    $nicemailparagraph(p=mailp5;l=mailp6;spacing=mailspacing5)$
                                    $nicemailparagraph(p=mailp6;l=false)$
                                  </td>
                                </tr>
                              </table>
                          $else$
                            $nicemailparagraph(p=mailp1;l=mailp2;spacing=mailspacing1)$
                            $nicemailparagraph(p=mailp2;l=mailp3;spacing=mailspacing2)$
                            $nicemailparagraph(p=mailp3;l=mailp4;spacing=mailspacing3)$
                            $nicemailparagraph(p=mailp4;l=mailp5;spacing=mailspacing4)$
                            $nicemailparagraph(p=mailp5;l=mailp6;spacing=mailspacing5)$
                            $nicemailparagraph(p=mailp6;l=false)$
                          $endif$
                          </div>
                        </td>
                      </tr>
                    </table>
                    <!-- // End Module: Standard Content \\ -->
                  </td>
                </tr>
              </table><!-- // End Template Body \\ -->
            </td>
          </tr>

          $if(footer)$
          <tr>
            <td>
              <!-- // Begin Template Footer \\ -->

              <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateFooter" style="border-top: 1px solid $nicemailbordercolor()$;">
                <tr>
                  <td valign="top" class="footerContent">
                    <!-- // Begin Module: Standard Footer \\ -->

                    <table border="0" cellpadding="0" cellspacing="0" width="100%">
                      <tr>

                        <td colspan="2" valign="top">
                          <br />
                          <div style="color: $nicemailfgcolour()$; font-family: $nicemailfont()$; font-style: italic; font-size: 13px; line-height: 16px; text-align: justify; margin: 0px 32px;padding:32px 0px;">
                            $footer:noescape()$
                          </div>

                          <br />
                        </td>
                      </tr>
                    </table>
                    <!-- // End Module: Standard Footer \\ -->
                  </td>
                </tr>
              </table><!-- // End Template Footer \\ -->
            </td>
          </tr>
          $else$
          <tr>
            <td>
              <div style="padding:32px; border-top: 1px solid $nicemailbordercolor()$;">
                <a style="text-decoration: none;" href="$ctxhostpart$">
                  <span class="h3" style="color: $nicemailfgcolour()$; display: block; font-family: $nicemailfont()$; font-size: 16px; font-weight: normal; line-height: 150%; text-align: left;">
                    $PoweredByScrive()$
                  </span>
                </a>
              </div>
            </td>
          </tr>
          $endif$
        </table>
        <br />
      </td>
    </tr>
  </table>
</center>
######
nicemailbgcolour=#FFFFFF
#####
nicemailfgcolour=#505050
#####
nicemaillogo=*
  $if(ispreview)$
    $if(documentid)$
      $if(signatorylinkid)$
        $ctxhostpart$/email_logo/$documentid$/$signatorylinkid$/$logoAdler32$
      $else$
        $ctxhostpart$/email_logo/$logoAdler32$
      $endif$
    $else$
      $ctxhostpart$/email_logo/$logoAdler32$
    $endif$
  $else$
    cid:logo-$logoAdler32$.png
  $endif$
#####
nicemailheaderbackground=$brandcolor$
#####
nicemailbordercolor=#cccccc;
#####
nicemailfont=$font$
#####
#####
nicemailparagraph =
$if(p)$
  $p:noescape()$
$endif$
$if(l)$
  $if(spacing)$
    $spacing:noescape()$
  $else$
    <br/>
    <br/>
  $endif$
$endif$
####
nicebutton=
<table width="100%" border="0" cellspacing="0" cellpadding="0">
  <tr>
    <td>
      <table border="0" cellspacing="0" cellpadding="0">
        <tr>
          <td align="center"
              bgcolor="$actioncolor$">
            <a $if(link)$href="$link$"$endif$
               class="button-blue notclickableinpreview"
               style="font-size: 16px;
                      font-family: $nicemailfont()$;
                      color: #ffffff;
                      color: $actiontextcolor$;
                      text-decoration: none;
                      display: inline-block;
                      padding: 9px 22px;
                      border: 1px solid $actioncolor$;">
              <!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]-->$text$<!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]-->
            </a>
          </td>
        </tr>
      </table>
    </td>
  </tr>
</table>
####
