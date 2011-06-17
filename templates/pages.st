####
signupPageView =
<div class="newdesign">
<div class="try-skrivapa">
  <div class="left">
       <h2>$_signupPageViewBodyHead()$</h2>
       <p>$_signupPageViewBodyBody()$</p>
  </div>
  <div class="right">
     <form action="" method="post">
         <input type="email"  infotext="$_signupPageViewEmail()$" name="email" /> <a class="submit btn-small green2 center float-right" href="#"><p>$_signupPageViewCreateAccount()$</p></a>
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
       <h2>$_signupVipPageViewHead()$</h2>
       <p>$_signupVipPageViewBody()$</p>
  </div>
  <div class="right">
     <form action="" method="post">
         <input type="email"  infotext="$_signupVipPageViewEmail()$" name="email" class="shiny"/> <a class="submit btn-small green2 center float-right" href="#"><p>$_signupVipPageViewCreateAccount()$</p></a>
         <input type="submit" class="hidden"/>
    </form>
  </div>
</div>
</div>
####
activatePageViewNotValidLink=
    <div class="centerdivnarrow">
      <p>$_activatePageViewNotValidLinkBody()$</p>
      <form action="" method="post">
        <table>
          <tr>
            <td>$_activatePageViewNotValidLinkEmail()$</td>
            <td><input type="email" name="email" value="$if(email)$$email$$endif$" /></td>
          </tr>
        </table>
        <input type="submit" name="resendActivate" value="$_activatePageViewNotValidLinkSend()$"  class="validateEmail" />
      </form>
    </div>
####
