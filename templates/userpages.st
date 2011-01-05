####    Pages for users - usual from my account      
viewSubaccounts = 
    <form method="post" action="$subaccountslink$">
     <h1>Underkonton</h1>
     <table class="doctable" cellspacing="0">
      <col/>
      <col/>
      <col/>
      <col/>
      <col/>
      <thead>
       <tr>
        <td><a href="#" id="all">Alla</a></td>
        <td></td>
        <td>Namn</td>
        <td>E-post</td>
        <td>Övrigt</td>
        <td></td>
       </tr>
      </thead>
      <tfoot>
       <tr>
        <td colspan="6" style="text-align: right; overflow: hidden;">
          <div class="floatleft">
           <input type="submit" class="button" name="remove" value="Radera"/>
          </div>
          <div class="clearboth"/>
         </td>
       </tr>
      </tfoot>
     
      <tbody id="selectable">
       $subusers:{subuser |
           <tr class="ui-state-default">
             <td class="tdleft">
              <input type="checkbox" name="doccheck" value="$subuser.usvId$" class="check" />
             </td>
             <td><img width="17" height="17" src=""/></td>
             <td>$subuser.usvFullname$ </td>
             <td>$subuser.usvEmail$</td>
             <td> - </td>
             <td class="tdright"></td>
             </tr>
                  }$
      </tbody>
     </table><br/>
      <table>
        <tr> 
          <td>Namn:</td> 
          <td><input type="text" name="fullname"/></td> 
        </tr>
        <tr>
          <td>E-post:</td> 
          <td><input type="email" name="email"/></td> 
        </tr>
        <tr> 
          <td><input class="button" id="create" type="submit" name="create" value="Skapa ny"/></td>
          <td></td>
        </tr>
      </table>
    </form>


###
pageAcceptTOS =
     <form method="post" action="/accepttos" id="toscontainer" class="overlay">
      <a class="close"> </a>                  
      <h2>Vänligen acceptera användarvillkoren</h2>
      $tostext$
     <br/>
     <input type="checkbox" name="tos" id="tos"/>Jag har läst och accepterar SkrivaPå Allmänna Villkor<br/>
     <div class="buttonbox">
      <input class="submiter button" type="submit" value="Skicka"/>
     </div> 
    </form>
####
showUser = 
    <div class="accounttable">

     <h1>$fullname$</h1>



      <div>
       <form action="$linkaccount$" method="post">
        <table>
         <tr><td>Namn:</td>
             <td><input type="text" name="fullname" value="$fullname$"/></td>
         </tr>
         <tr><td>E-post:</td>
             <td>$email$</td>
         </tr>
         <tr><td>Företagsnamn:</td>
             <td><input type="text" name="companyname" value="$companyname$" /></td>
         </tr>
         <tr><td>Organisationsnummer:</td>
             <td><input type="text" name="companynumber" value="$companynumber$"/></td>
         </tr>
         <tr><td>Faktureringsadress:</td>
             <td><input type="text" name="invoiceaddress" value="$invoiceaddress$"/></td>
         </tr>

        </table>
        <p>Du delar dina dokument med följande användare:</p>
        <ul>
          $viewers:{viewer|<li> $viewer$</li> }$ 
          <li><input type="text" name="newvieweremail" infotext="Skriv e-post för att lägga till fler" size="28" /></li>
        </ul>

        <input class="button" type="submit" value="Spara ändringar"/>
      </form>
     </div>



     <div>
      <form action="$linkaccountpassword$" method="post">
       <table>
        <tr><td>Nuvarande lösenord:</td>
            <td><input type="password" name="oldpassword" autocomplete="off" /></td>
        </tr>
        <tr><td>Nytt lösenord:</td>
            <td><input type="password" name="password" autocomplete="off" /></td>
        </tr>
        <tr><td>Upprepa nytt lösenord:</td>
            <td><input type="password" name="password2" autocomplete="off" /></td>
        </tr>
       </table>
       <input class="button" type="submit" value="Ändra lösenord"/>
      </form>
     </div>

     <div>
      <a href="$linksubaccount$">Underkonton</a>
     </div>
    </div>


####