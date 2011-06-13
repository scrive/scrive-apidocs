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
         <input type="email"  infotext="Fyll i din e-postadress" name="email" /> <a class="submit btn-small green2 center float-right" href="#"><p>Skapa konto !</p></a>
         <input type="submit" class="hidden" />
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
         <input type="email"  infotext="Fyll i din e-postadress" name="email" class="shiny"/> <a class="submit btn-small green2 center float-right" href="#"><p>Skapa konto !</p></a>
         <input type="submit" class="hidden"/>
    </form>
  </div>
</div>
</div>
####
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
        <input type="submit" name="resendActivate" value="Skicka"  class="validateEmail" />
      </form>
    </div>
####
