nicemail=
<center style="background-color: $nicemailemailbackgroundcolour()$;">
  <table border="0" cellpadding="0" cellspacing="0" height="100%" width="100%" id="backgroundTable" style="height: 100%; margin: 0; padding: 0; width: 100%;">
    <tr>
      <td align="center" valign="top">
        <!-- // Begin Template Header \\ -->
        <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateHeader" style="background-color: transparent; border-bottom: 0; padding: 24px 0 8px;">
          <tr>
            <td class="headerContent" style="color: $nicemailbordercolour()$; font-family: Helvetica Neue, Arial, sans-serif; font-size: 40px; font-weight: bold; line-height: 100%; padding: 0; text-align: left; vertical-align: middle">
              <!-- // Begin Module: Standard Header Image \\ -->
              <img src="$nicemaillogo()$" style="border: none; font-size: 14px; font-weight: bold; height: auto; line-height: 100%; outline: none; text-decoration: none; text-transform: capitalize; max-width:600px; margin-top: 50px; margin-bottom: 20px;" id="headerImage campaign-icon" />
              <!-- // End Module: Standard Header Image \\ -->
            </td>
          </tr>
        </table><!-- // End Template Header \\ -->
      </td>
    </tr>

    <tr>
      <td align="center" valign="top">
        <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateContainer" style="border: 4px solid $nicemailbordercolour()$; border-top: auto; background-color: $nicemailbgcolour()$;">
          <tr>
            <td></td>
          </tr>

          <tr>
            <td align="center" valign="top">
              <!-- // Begin Template Body Top \\ -->

              <table border="0" cellpadding="40" cellspacing="0"width="600" id="templateTop" style="border-bottom-color: $nicemailbordercolour()$; border-bottom-width: 1px; border-bottom-style: solid;">
                <tr>
                  <td valign="top" class="topContent">
                    <table border="0" cellpadding="0" cellspacing="0" width="100%" $if(ispreview)$style="border-collapse: separate; border-spacing: $borderspacing()$"$endif$>
                      <tr>
                        <td valign="top">
                          <div>
                            <span class="h1" style="color: #364963; display: block; font-family: $nicemailheaderfont()$; font-size: 28px; line-height: 120%; margin-bottom: 8px; text-align: left">
                              $if(mailheader)$ $mailheader$ $endif$
                            </span>
                            <a style="text-decoration: none;" href="$ctxhostpart$">
                              <span class="h3" style="color: #7a94b8; display: block; font-family: $nicemailheaderfont()$; font-size: 16px; font-weight: normal; line-height: 150%; text-align: left">
                                $_poweredByScrive()$
                              </span>
                            </a>
                          </div>
                        </td>
                      </tr>
                    </table>
                  </td>
                </tr>
              </table><!-- // Begin Template Body \\ -->

              <table border="0" cellpadding="0" cellspacing="40" width="600" id="templateBody" style="border-bottom-color: $nicemailbordercolour()$; border-bottom-width: 1px; border-bottom-style: solid; $if(ispreview)$border-collapse: separate; border-spacing: $borderspacing()$$endif$">
                <tr>
                  <td valign="top" class="bodyContent" style="background-color: $nicemailbgcolour()$">
                    <!-- // Begin Module: Standard Content \\ -->

                    <table border="0" cellspacing="0" width="100%">
                      <tr>
                        <td valign="top">
                          <div style="color: $nicemailfgcolour()$; font-family: $nicemailfont()$; font-size: 16px; line-height: 150%; text-align: left">
                            $if(previewLink)$
                              <div style="color: #333; font-family: $nicemailfont()$; font-size: 16px; line-height: 0; text-align: left; float:left;margin-right:32px;border:1px solid $nicemailbordercolour()$;">
                                <img src="$ctxhostpart$$previewLink$" style="border: none; font-size: 14px; font-weight: bold; height: auto; line-height: 100%; outline: none; text-decoration: none; text-transform: capitalize; display: inline; max-width:150px;"/>
                              </div>
                              <div style="float: right; width: 330px;">
                                $nicemailparagraph(p=mailp1;l=mailp2;spacing=mailspacing1)$
                                $nicemailparagraph(p=mailp2;l=mailp3;spacing=mailspacing2)$
                                $nicemailparagraph(p=mailp3;l=mailp4;spacing=mailspacing3)$
                                $nicemailparagraph(p=mailp4;l=mailp5;spacing=mailspacing4)$
                                $nicemailparagraph(p=mailp5;l=mailp6;spacing=mailspacing5)$
                                $nicemailparagraph(p=mailp6;l=false)$
                              </div>
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
            <td align="center" valign="top">
              <!-- // Begin Template Footer \\ -->

              <table border="0" cellpadding="0" cellspacing="0" width="600" id="templateFooter" style="background-color: $nicemailbgcolour()$; border-top: 0;">
                <tr>
                  <td valign="top" class="footerContent">
                    <!-- // Begin Module: Standard Footer \\ -->

                    <table border="0" cellpadding="0" cellspacing="0" width="100%">
                      <tr>

                        <td colspan="2" valign="top">
                          <br />
                          <div style="color: #999; font-family: $nicemailfont()$; font-style: italic; font-size: 13px; line-height: 125%; text-align: justify; padding: 0 40px">
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
          $endif$
        </table>
        <br />
      </td>
    </tr>
  </table>
</center>
######
borderspacing=40px 30px
######
nicemailbgcolour=$if(!companybrand.barsbackground)$#FFFFFF$else$$companybrand.barsbackground$$endif$
#####
nicemailfgcolour=$if(!companybrand.barstextcolour)$#333333$else$$companybrand.barstextcolour$$endif$
#####
nicemaillogo=$if(!companybrand.logo)$$ctxhostpart$/img/logo_email.png$else$$ctxhostpart$$companybrand.logoLink$$endif$
#####
nicemailbordercolour=$if(!companybrand.bordercolour)$#dee4ed$else$$companybrand.bordercolour$$endif$
#####
nicemailheaderfont=$if(!companybrand.headerfont)$Helvetica Neue, Arial, sans-serif$else$$companybrand.headerfont$$endif$
#####
nicemailfont=$if(!companybrand.font)$Helvetica Neue, Arial, sans-serif$else$$companybrand.font$$endif$
#####
nicemailbuttoncolour=$if(!companybrand.buttoncolour)$215$else$$companybrand.buttoncolour$$endif$
#####
nicemailemailbackgroundcolour=$if(!companybrand.emailbackgroundcolour)$#fff$else$$companybrand.emailbackgroundcolour$$endif$
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
<a href="$link$" class="button-blue" style="background: hsl($nicemailbuttoncolour()$, 30%, 35%); border: 2px solid hsl($nicemailbuttoncolour()$, 30%, 23%); -webkit-box-shadow: inset hsl($nicemailbuttoncolour()$, 30%, 60%) 0 0 0 1px; -moz-box-shadow: inset hsl($nicemailbuttoncolour()$, 30%, 60%) 0 0 0 1px; -ms-box-shadow: inset hsl($nicemailbuttoncolour()$, 30%, 60%) 0 0 0 1px; -o-box-shadow: inset hsl($nicemailbuttoncolour()$, 30%, 60%) 0 0 0 1px; box-shadow: inset hsl($nicemailbuttoncolour()$, 30%, 60%) 0 0 0 1px; text-decoration: none; max-height: 16px; font: 16px/16px Helvetica Neue, Arial, sans-serif; padding: 12px 20px; margin: 0; color: #fff; text-shadow: hsla(0, 0, 0, 0.5) 0 1px 0; text-align: center; -webkit-transition: 0.15s ease-in-out; -moz-transition: 0.15s ease-in-out; -ms-transition: 0.15s ease-in-out; -o-transition: 0.15s ease-in-out; transition: 0.15s, ease-in-out; cursor: pointer; -moz-user-select: none; -ms-user-select: none; -o-user-select: none; -webkit-user-select: none; user-select: none; display: inline-block; -webkit-transform: translatez(0); -moz-transform: translatez(0); -ms-transform: translatez(0); -o-transform: translatez(0); transform: translatez(0); zoom: 1">
  $text$
</a>
