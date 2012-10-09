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
        <input type="submit" name="resendActivate" value="$_send()$"  class="validateEmail" />
      </form>
    </div>
####
