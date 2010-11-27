#########             UTILS           #######
####
strong=<strong>$it$</strong>
###
morethenonelist= $list:strong(); separator=", "$ och $last:strong()$
###
###
nomoretheneonelist= $list:strong()$
###
loginBox=  
<div>
    <div id="login">
     <form action="$LinkLogin()$" method="post">
      <table>
        <tr>
          <td>E-mail:</td> 
          <td><input type="email" name="email" autocomplete="off" class="noflash"/></td> 
        </tr>
        <tr> 
          <td>Lösenord:</td> 
          <td><input type="password" name="password" autocomplete="off"/></td> 
        </tr>
    <tr>
          <td>
          </td>
          
          <td style="display: none">
            <input type="checkbox" id="rememberme" name="rememberme"/>
            <label for="rememberme">Kom ihåg mig</label>
          </td>
    </tr>
        <tr> 
          <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"/></td>
          <td>
           <a href="$LinkForgotPassword()$"> Glömt lösenord</a>
          </td>
        </tr>
      </table>
    </form>
    </div>
   </div>
###
customFooter=  <p>
                 Hälsningar
                 <br/>
                $creatorname$
                </p>
###
poweredBySkrivaPaPara= 
    <p>
     <small>Med vänliga hälsningar<br/><a href="$ctxhostpart$">SkrivaPå</a></small>
    </p>
#####
wrapHTML= <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
       $body$
     </body>
    </html>

####
makeEditable=<div class="editable" name="$name$">$this$</div>         
####
replaceOnEdit= <span> 
                 <span class="replacebynextonedit"> $this$ </span> 
                 <span style="display:none"> $with$ </span>  
               </span>  
#####
LinkLogin=/login
###
LinkForgotPassword=/amnesia
###
LinkLandpageSaved=/landpage/signedsave/$documentid$/$signatorylinkid$
###