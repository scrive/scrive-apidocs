####
showUserSecurity =
<form action="$linksecurity$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li><a href="/account">Användaruppgifter</a></li>
       <li class="active"><a href="/account/security">Lösenord</a></li>
       <li><a href="/account/subaccount">Underkonton</a></li>
       <li><a href="/account/sharing">Delning</a></li>
   </ul>
   <div class="tab-container">
       <div class="tab-content account">
           <div class="col">
               <div class="account-header">
                   <h2>Byt lösenord</h2>
               </div>
               <div class="account-body">
                   <table>
                       <tr>
                           <td>Nuvarande lösenord:</td>
                           <td><input type="password" name="oldpassword" /></td>
                       <tr>
                           <td>Nytt lösenord:</td>
                           <td><input type="password" name="password" /></td>
                       </tr>
                       <tr>
                           <td>Upprepa nytt lösenord:</td>
                           <td><input type="password" name="password2" /></td>
                       </tr>
                   </table>
               </div>
             <div class="account-footer">
                <input type="submit" class="hidden"/>
                <a href="#" class="btn-small green submit">
                   <div class="left"></div>
                   <div class="label">
                       <p>Spara lösenord</p>
                   </div>
                   <div class="right"></div>
                </a>
             </div>
           </div>
           <div class="clearfix"></div>
       </div>
   </div>
</div>
####
