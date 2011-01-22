#########             UTILS           #######
####
strong=
    <strong>$it$</strong>
###
morethenonelist=
    $list:strong(); separator=", "$ och $last:strong()$
###
###
nomoretheneonelist=
    $list:strong()$
###
loginBox=
    <div>
      <div id="login">
        <form action="$LinkLogin()$" method="post">
          <table>
            <tr>
              <td>E-mail:</td>
              <td><input type="email" name="email" autocomplete="off" class="noflash"></td>
            </tr>
            <tr>
              <td>Lösenord:</td>
              <td><input type="password" name="password" autocomplete="off"></td>
            </tr>
            <tr>
              <td>
              </td>
              <td style="display: none">
                <input type="checkbox" id="rememberme" name="rememberme">
                <label for="rememberme">Kom ihåg mig</label>
              </td>
            </tr>
            <tr>
              <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"></td>
              <td>
                <a href="$LinkForgotPassword()$"> Glömt lösenord</a>
              </td>
            </tr>
          </table>
        </form>
      </div>
    </div>
###
customFooter=
    <p>
      Hälsningar
      <br>
      $creatorname$
    </p>
###
poweredBySkrivaPaPara=
    <p>
      <small>Med vänliga hälsningar<br/><a href="$ctxhostpart$">SkrivaPå</a></small>
    </p>
#####
wrapHTML=
    <html>
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      </head>
      <body>
        $body:noescape()$
      </body>
    </html>
####
makeEditable=
    <div class="editable" name="$name$">$this:noescape()$</div>
####
replaceOnEdit=
    <span>
      <span class="replacebynextonedit"> $this:noescape()$ </span>
      <span style="display:none"> $with:noescape()$ </span>
    </span>
#####
actionButton=
    <form method="POST" action="$action$">
      <input type="submit" value="$buttonname$">
    </form>
#####
LinkLogin=/login
###
LinkForgotPassword=/amnesia
###
LinkLandpageSaved=/landpage/signedsave/$documentid$/$signatorylinkid$
###
###
join=$fst$$snd$
###
option=<option value="$oValue$" $if(oSelected)$ SELECTED $endif$ > $oText$ </option>
###
options=$it:{n | $option(oValue=n.oValue;oText=n.oText;oSelected=n.oSelected)$}$
###
input=<input style="width:50px" type="$iType$" name="$iName$" value="$iValue$">
###
showmoney=$main$.$rest$
###
br=$it:noescape()$<br>
###
span=<span>$it:noescape()$</span>
###
td=<td style="text-align:center">$it:noescape()$</td>
###
###
th=<th style="width:60px;text-align:center">$it:noescape()$</th>
###
spanNoEscape=<span>$it:noescape()$</span>
####
