####
showUserSecurity =
<form action="$linksecurity$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li><a href="/account">Användaruppgifter</a></li>
       <li><a href="/account/subaccount">Underkonton</a></li>
       <li><a href="/account/sharing">Delning</a></li>
       <li class="active"><a href="/account/security">Säkerhet</a></li>
   </ul>
   <div class="tab-container">
       <div class="tab-content account">
           <div>
               <div class="account-body">
                   <h2>Byt lösenord</h2>
                   <table>
                       <tr>
                           <td>Nuvarande lösen.:</td>
                           <td><input type="password" name="oldpassword" /></td>
                       <tr>
                           <td>Nytt lösenord:</td>
                           <td><input type="password" name="password" /></td>
                       </tr>
                       <tr>
                           <td>Upprepa nytt lösen.:</td>
                           <td><input type="password" name="password2" /></td>
                       </tr>
                       <tr>
                           <td></td>
                           <td>
                               <a href="#" class="btn-small green float-right submit">
                                 <div class="left"></div>
                                 <div class="label">
                                   <span>Spara</span>
                                 </div>
                                 <div class="right"></div>
                               </a>
                               <input class="hidden" type="submit" value="" />
                           </td>
                       </tr>
                   </table>
                   <div>
                       
                   </div>
               </div>
           </div>
           <div class="clearfix"></div>
       </div>
   </div>
</div>
####
