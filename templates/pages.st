###
restartButtonName=Starta om dokument
######
uploadPageContent=
    <div class="centerdivnarrow" style="margin-top: 100px;">
      <form action="/d" method="post" enctype="multipart/form-data">
        <span class="small">Välj dokument (endast PDF)</span><br>
        <input class="multi" maxlength="1" type="file" name="doc" accept="application/pdf">
        <input class="bigbutton" type="submit" value="Ladda upp">
      </form>
    </div>
####
firstPageContent=
    <div>
      <div id="firstPageLeft">
        <h1>Snabbt, smidigt och säkert</h1>
        <p>SkrivaPå är en tjänst för elektronisk signering och arkivering. Spara tid och minska onödiga kostnader med enklare och smidigare avtalshantering. Skapa ett konto gratis och testa tjänsten redan idag! Vi bjuder på de 5 första avtalen.</p>
        <p><a href="$signuplink$" class="button">Skapa konto</a></p>
      </div>
      <div id="firstPageRight">
        <object>
          <embed src="https://secure.vimeo.com/moogaloop.swf?clip_id=17638316&server=vimeo.com&show_title=0&show_byline=0&show_portrait=0&color=00ADEF&fullscreen=1" type="application/x-shockwave-flash" allowfullscreen="true" allowscriptaccess="always" width="520" height="319">
          </embed>
        </object>
      </div>
    </div>
#####
newPasswordPage=
    <form action="" method="post">
      <table>
          <tr>
            <td>Lösenord:</td>
            <td><input type="password" name="password" autocomplete="off"></td>
          </tr>
          <tr>
            <td>Upprepa lösenord:</td>
            <td><input type="password" name="password2" autocomplete="off"></td>
          </tr>
      </table>
      <input class="button" type="submit" value="Ändra lösenord">
    </form>
####
signupPageView =
    <div class="centerdivnarrow">
      <form action="" method="post">
        <table>
          <tr>
            <td>E-mail:</td>
            <td><input type="email" name="email" value="" /></td>
          </tr>
        </table>
        <input type="submit" class="validateEmail" value="Skapa konto">
      </form>
    </div>
####
activatePageView=
    <form action="" method="post">
      <h2>Vänligen acceptera användarvillkoren</h2>
      <div style="height:480px;overflow-y:scroll;border:1px solid #EEEEEE">
        $tostext:noescape()$
        <br>
      </div>
      <br>
      <input type="checkbox" name="tos" class="addremovecheckbox" rel=".passwordAndSubmit" location=".passwordAndSubmitVisible" oldlocation=".passwordAndSubmitHidden" value="off" autocomplete="off">
      Jag har läst och accepterar SkrivaPå Allmänna Villkor<br/><br/>
      <div class="passwordAndSubmitVisible"> </div>
    </form>
    <div class="passwordAndSubmitHidden" style="display:none">
      <div class="passwordAndSubmit">
        <table><tbody>
            <tr>
              <td>Namn:</td>
              <td><input type="text" name="name" autocomplete="off"></td>
            </tr>
            <tr>
              <td>Välj lösenord:</td>
              <td><input type="password" name="password" autocomplete="off"></td>
            </tr>
            <tr>
              <td>Upprepa lösenord:</td>
              <td><input type="password" name="password2" autocomplete="off"></td>
            </tr>
        </tbody></table>
        <input class="button" type="submit" value="Gå vidare"/>
      </div>
    </div>
###
activatePageViewNotValidLink=
    <div class="centerdivnarrow">
      <p> Begklagar, din aktiveringslänk är äldre än 12 timmar. Vänligen fyll i
        din mailadress så skickar vi en ny aktiveringslänk omedelbart.
      </p>
      <form action="" method="post">
        <table>
          <tr>
            <td>E-mail:</td>
            <td><input type="email" name="email" value="" /></td>
          </tr>
        </table>
        <input type="submit" value="Skicka"  class="validateEmail">
      </form>
    </div>
####
errorPage=
   <div style="width:500px;margin-top:100px;border:5px solid #AAAACC;margin-left: auto;margin-right: auto;text-align:center">
     <div style="padding:30px">
       <div style="font-size:19pt;font-weight:bold" > Oops, ett fel har uppstått </div>
       <BR>
       <BR>
       <div> Oroa dig inte, vi tittar på problemet.</div>
     </div>
   </div>
####
