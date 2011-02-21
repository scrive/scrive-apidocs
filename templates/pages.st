###
restartButtonName=Starta om dokument
####
newPasswordPage=     
    Vänligen välj lösenord.
    <form action="" method="post">
      <table><tbody>
          <tr>
            <td>Nytt lösenord:</td>
            <td><input type="password" name="password" autocomplete="off"></td>
          </tr>
          <tr>
            <td>Upprepa lösenord:</td>
            <td><input type="password" name="password2" autocomplete="off"></td>
          </tr>
      </tbody></table>
      <input class="button" type="submit" value="Spara lösenord">
    </form>
####
signupPageView =
<div class="newdesign">
<div class="try-skrivapa">
  <div class="left">
       <h2>Gratis SkrivaPå till och med 31 maj!</h2>
       <p>Allt inkluderat. Standardkonto, fria transaktioner och fri lagring.</p>
  </div>
  <div class="right">
     <form action="" method="post">
         <input type="email" value="Fyll i din e-postadress" name="email" class="shiny infotext"> <a class="submit btn-small green2 center float-right" href="#"><p>Skapa konto !</p></a>
         <input type="submit" class="hidden">
    </form>
  </div>
</div>
</div>
########
signupVipPageView =
<div class="newdesign">
<div class="try-skrivapa">
  <div class="left">
       <h2>Gratis SkrivaPå till och med 31 december!</h2>
       <p>Allt inkluderat. Standardkonto, fria transaktioner och fri lagring.</p>
  </div>
  <div class="right">
     <form action="" method="post">
         <input type="email" value="Fyll i din e-postadress" name="email" class="shiny infotext"> <a class="submit btn-small green2 center float-right" href="#"><p>Skapa konto !</p></a>
         <input type="submit" class="hidden">
    </form>
  </div>
</div>
</div>
####
activatePageView=
    <form action="" method="post" style="border:1px solid;border-color:#D0D0D0 #BDBDBD #C0C0C0;padding:7px;background:#ffffff">
      <h2>Vänligen acceptera användarvillkoren</h2>
      <div style="height:480px;overflow-y:scroll;border:1px solid #EEEEEE;padding:4px">
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
              <td><input type="text" name="name" autocomplete="off" value="$if(name)$$name$$endif$"></td>
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
            <td><input type="email" name="email" value="$if(email)$$email$$endif$" /></td>
          </tr>
        </table>
        <input type="submit" name="resendActivate" value="Skicka"  class="validateEmail">
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
