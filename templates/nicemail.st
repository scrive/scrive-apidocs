nicemail=
<center class="rendered-email">
  <table border="0" cellpadding="0" cellspacing="0" width="100%" id="backgroundTable" style="margin: 0; padding: 0; width: 100%; vertical-align: top; background-color: $nicemailemailbackgroundcolour()$; line-height: 0px;">
    <tr>
      <td align="center" valign="top">
        <!-- // Begin Template Header \\ -->
        <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateHeader" style="background-color: transparent; border-bottom: 0;">
          <tr>
            <td class="headerContent" style="color: $nicemailbordercolour()$; font-family: Helvetica Neue, Arial, sans-serif; font-size: 40px; font-weight: bold; line-height: 100%; padding: 0; text-align: left; vertical-align: middle" >
              <!-- // Begin Module: Standard Header Image \\ -->
              <table>
                <tr>
                  <td style="padding-top: 48px; padding-bottom: 24px;">
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
        <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateContainer" style="border: 4px solid $nicemailbordercolour()$; background-color: $nicemailbgcolour()$;">
          <tr>
            <td></td>
          </tr>

          <tr>
            <td align="center" valign="top">
              <!-- // Begin Template Body \\ -->

              <table border="0" cellpadding="0" cellspacing="40" width="600" id="templateBody" style="border-bottom-color: $nicemailbordercolour()$; border-bottom-width: 1px; border-bottom-style: solid; border-collapse: separate;">
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
                                        <td style="height: 106px; color: #333; font-family: Helvetica Neue, Arial, sans-serif; font-size: 16px; line-height: 0; text-align: left; border:1px solid #dee4ed; width: 150px;">
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

              <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateFooter" style="background-color: $nicemailbgcolour()$; border-top: 0;">
                <tr>
                  <td valign="top" class="footerContent">
                    <!-- // Begin Module: Standard Footer \\ -->

                    <table border="0" cellpadding="0" cellspacing="0" width="100%">
                      <tr>

                        <td colspan="2" valign="top">
                          <br />
                          <div style="color: $if(!textcolor)$#999999$else$$textcolor$$endif$; font-family: $nicemailfont()$; font-style: italic; font-size: 13px; line-height: 16px; text-align: justify; margin: 20px 40px;">
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
              <div style="margin: 20px 40px;">
                $poweredByScrive()$
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
nicemailbgcolour=$if(!background)$#FFFFFF$else$$background$$endif$
#####
nicemailfgcolour=$if(!textcolor)$#333333$else$$textcolor$$endif$
#####
nicemaillogo=$if(ispreview)$$if(!logo)$$ctxhostpart$/img/logo_email.png$else$$ctxhostpart$$logoLink$$endif$$else$cid:logo.png$endif$
#####
nicemailbordercolour=$if(!bordercolour)$#dee4ed$else$$bordercolour$$endif$
#####
nicemailfont=$if(!font)$Helvetica Neue, Arial, sans-serif$else$$font$$endif$
#####
#####
nicemailemailbackgroundcolour=$if(!emailbackgroundcolour)$#fff$else$$emailbackgroundcolour$$endif$
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
              bgcolor="$if(!buttoncolour)$#53b688$else$$buttoncolour$$endif$">
            <a $if(link)$href="$link$"$endif$
               class="button-blue notclickableinpreview"
               style="font-size: 16px;
                      font-family: Helvetica Neue, Arial, sans-serif;
                      color: #ffffff;
                      text-decoration: none;
                      display: inline-block;
                      padding: 9px 22px;
                      border: 1px solid $if(!buttoncolour)$#53b688$else$$buttoncolour$$endif$;">
              <!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]-->$text$<!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]--><!--[if mso]>&nbsp;<![endif]-->
            </a>
          </td>
        </tr>
      </table>
    </td>
  </tr>
</table>
####
poweredByScrive=
<a style="text-decoration: none;" href="$ctxhostpart$">
  <span class="h3" style="color: $if(!textcolor)$#7a94b8$else$$textcolor$$endif$; display: block; font-family: $nicemailfont()$; font-size: 16px; font-weight: normal; line-height: 150%; text-align: left; margin-left: 10px;">
    $PoweredByScrive()$
  </span>
</a>
####
