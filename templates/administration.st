###
adminsmain=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
    jQuery(".admin").append(new Admin({isAdmin: $if(admin)$true$else$false$endif$}).el());
</script>
</div>
</div>
###
adminuser=
<div class="mainContainer" >
  <div class="body-container">
    <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly/#useradmin">Go back to user admin page</a>
    <br/>
    <br/>

      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="active float-left"><a href="/adminonly/useradmin/$user.id$">Användaruppgifter</a></li>
	  <li class="float-left"><a href="/adminonly/useradmin/payments/$user.id$">Payment info</a></li>
	  <li class="float-left"><a href="/adminonly/useradmin/usagestats/$user.id$">Statistics</a></li>
      <li class="float-left"><a href="/adminonly/useradmin/documents/$user.id$">Documents</a></li>

          <li class="float-right"/>
	</ul>
	<div class="tab-container">
	  <div class="tab-content account">
           <form action="" method="POST">
	    <table><tbody>
		<tr> <th style="width:200px"></th> <th> </th></tr>
		<tr> <td style="padding-top:10px"><strong>User data</strong></td> <td> </td></tr>
		<tr> <td>First Name:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.fstname$"  name="userfstname" />
		  </td>
		</tr>
		<tr> <td>Secound Name:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.sndname$"  name="usersndname" />
		  </td>
		</tr>
		<tr> <td>Pers Nr:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.personalnumber$"  name="userpersonalnumber" />
		  </td>
		</tr>
		<tr> <td style="padding-top:10px" ><strong>Contact</strong></td>
		  <td>
		  </td>
		</tr>
		<tr> <td>Phone:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0"  value="$user.phone$" name="userphone" />
		  </td>
		</tr>
		<tr> <td>Email:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0"  value="$user.email$" name="useremail" />
		  </td>
		</tr>
                <tr> <td style="padding-top:10px" ><strong>Company</strong></td>
                 <td>
                 </td>
               </tr>
                $if(!user.iscompanyaccount)$
               <tr> <td>Company name:</td>
                 <td>
                   <input type="text" style="width:200px;margin:0" value="$user.companyname$"  name="usercompanyname" />
                 </td>
               </tr>
               <tr> <td>Company number:</td>
                 <td>
                   <input type="text" style="width:200px;margin:0" value="$user.companynumber$"  name="usercompanynumber" />
                 </td>
               </tr>
                $endif$

		<tr> <td>Company position:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.companyposition$"  name="usercompanyposition" />
		  </td>
		</tr>
    <tr> <td>Language:</td>
      <td><select name="userlang">
        <option $if(user.langsv)$selected=""$endif$ value="LANG_SV">Sweden (Swedish)</option>
        <option $if(user.langen)$selected=""$endif$ value="LANG_EN">Britain (English)</option>
      </select></td>
    </tr>
		<tr>
		  <td style="padding-top:10px" ><strong>Account</strong></td>
		  <td> </td>
		</tr>
		<tr> <td>Account type:</td>
		  <td>
		    $if(user.iscompanyaccount)$
        <p><a href="/adminonly/companyadmin/$company.companyid$">
         view company $company.companyname$
        </a></p>
        $endif$

		    <select name="useraccounttype">
		      <option $if(user.iscompanyaccount)$$if(user.iscompanyadmin)$selected=""$endif$$endif$ value="companyadminaccount">$company.companyname$ Admin Company Account $if(!user.iscompanyaccount)$(Creates a new company)$endif$</option>
		      <option $if(user.iscompanyaccount)$$if(!user.iscompanyadmin)$selected=""$endif$$endif$ value="companystandardaccount">$company.companyname$ Standard Company Account $if(!user.iscompanyaccount)$(Creates a new company)$endif$</option>
		      <option $if(!user.iscompanyaccount)$selected=""$endif$value="privateaccount">Private Account $if(user.iscompanyaccount)$(May leave an empty company)$endif$</option>
		    </select>
		  </td>
		</tr>
               <tr> <td>Free user:</td>
                 <td>
                   <input type="checkbox" $if(user.isfree)$checked="checked"$endif$  name="freeuser" />
                 </td>
               </tr>

	    </tbody></table>
	    <br/>
	    <input type="submit" style="width:200px;font-weight:bold"
	    value="Change" name="change" />
    </form>
    <form action="/adminonly/useradmin/sendinviteagain" method="POST">
      <input type="hidden" name="userid" value="$user.id$" />
      <input type="submit" value="Resend Invitation" style="width:200px;font-weight:bold" />
    </form>
	  </div>
	</div>
      </div>
</div>
</div>
####
adminuserpayments=
<div class="mainContainer" >
  <div class="body-container">
    <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly/#useradmin">Go back to user admin page</a>
    <br/>
    <br/>
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="float-left"><a href="/adminonly/useradmin/$userid$">Användaruppgifter</a></li>
	  <li class="active float-left"><a href="/adminonly/useradmin/payments/$userid$">Payment info</a></li>
	  <li class="float-left"><a href="/adminonly/useradmin/usagestats/$userid$">Statistics</a></li>
      <li class="float-left"><a href="/adminonly/useradmin/documents/$userid$">Documents</a></li>

          <li class="float-right"/>
	</ul>
      <div class="tab-container">
	<div class="tab-content account">
          <form method="POST">
	    <table><tbody>
		<tr> <th style="width:200px"></th> <th> </th></tr>
		<tr> <td style="padding-top:10px"><strong>Payment info</strong></td> <td> </td></tr>
                $if(haspaymentplan)$
                $if(companyid)$
		<tr>
                  <td><em>Warning:</em> </td>
		  <td>
                    This user has a personal (user) payment plan and is part of a company. Something has gone wrong.
		  </td>
		</tr>
                $endif$
                $if(recurlyplan)$
		<tr>
                  <td>Price plan on Recurly: </td>
		  <td>
                    <a href="https://$recurlysubdomain$.recurly.com/accounts/$accountcode$">$priceplan$</a>
		  </td>
                </tr>
		<tr>
                  <td>Status: </td>
		  <td>
                    $status$
		  </td>
		</tr>
                <tr>
                  <td>Migrate: </td>
                  <td>
                    <p>Migrate this user's subscription to another user/company.</p>
                    <p><select name="migratetype"><option value="userid">User</option><option value="companyid">Company</option></select> <input type="text" name="id" placeholder="userid/companyid"></input></p>
                    <p><input type="submit" name="migrate" value="Migrate"></input></p>
                    <p>Be very careful with this! </p>
                  </td>
                </tr>
                <tr>
                  <td>Delete: </td>
                  <td>
                    <p>Delete this subscription. Be careful! This will not delete it from Recurly. Please close the Recurly account first!</p>
                    <p><input type="submit" name="delete" value="Delete"></input></p>
                  </td>
		</tr>
                $else$
		<tr>
                  <td>Price plan (selected by admin):</td>
		  <td>
		    <select name="priceplan">
                      <option value="free"         $if(freeplan)$selected="selected"$endif$>Free (no plan)</option>
                      <option value="team"         $if(teamplan)$selected="selected"$endif$>Team</option>
                      <option value="form"         $if(formplan)$selected="selected"$endif$>Online Forms</option>
                      <option value="enterprise"   $if(enterpriseplan)$selected="selected"$endif$>Enterprise</option>
                      <option value="trial"        $if(trialplan)$selected="selected"$endif$>Trial</option>
                    </select>
		  </td>
		</tr>
		<tr>
                  <td>Status (selected by admin):</td>
		  <td>
		    <select name="status">
                      <option value="active"      $if(activestatus)$selected="selected"$endif$>Active</option>
                      <option value="overdue"     $if(overduestatus)$selected="selected"$endif$>Overdue</option>
                      <option value="canceled"    $if(canceledstatus)$selected="selected"$endif$>Canceled</option>
                      <option value="deactivated" $if(deactivatedstatus)$selected="selected"$endif$>Deactivated</option>
                    </select>
		  </td>
		</tr>
		<tr>
                  <td><input type="submit" style="width:200px;font-weight:bold"
	           value="Change" name="change" /></td>
		  <td>
		  </td>
		</tr>
                $endif$
                $else$
                $if(companyid)$
		<tr>
                  <td>Company price plan: </td>
		  <td>
                    This user is part of a company. <a href="/adminonly/companyadmin/payments/$companyid$">Visit the company price plan.</a>
		  </td>
		</tr>
                $else$
		<tr>
                  <td>Price plan (selected by admin):</td>
		  <td>
		    <select name="priceplan">
                      <option value="free"         $if(freeplan)$selected="selected"$endif$>Free (no plan)</option>
                      <option value="team"         $if(teamplan)$selected="selected"$endif$>Team</option>
                      <option value="form"         $if(formplan)$selected="selected"$endif$>Online Forms</option>
                      <option value="enterprise"   $if(enterpriseplan)$selected="selected"$endif$>Enterprise</option>
                      <option value="trial"        $if(trialplan)$selected="selected"$endif$>Trial</option>
                    </select>
		  </td>
		</tr>
		<tr>
                  <td>Status (selected by admin):</td>
		  <td>
		    <select name="status">
                      <option value="active"      $if(activestatus)$selected="selected"$endif$>Active</option>
                      <option value="overdue"     $if(overduestatus)$selected="selected"$endif$>Overdue</option>
                      <option value="canceled"    $if(canceledstatus)$selected="selected"$endif$>Canceled</option>
                      <option value="deactivated" $if(deactivatedstatus)$selected="selected"$endif$>Deactivated</option>
                    </select>
		  </td>
		</tr>
		<tr>
                  <td><input type="submit" style="width:200px;font-weight:bold"
	           value="Change" name="change" /></td>
		  <td>
		  </td>
		</tr>
                $endif$
                $endif$
	    </tbody></table>

          </form>
	</div>
      </div>
    </div>
</div>
</div>
####
admincompany=
<div class="mainContainer" >
  <div class="body-container">
    <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly#companyadmin">Go back to company admin page</a>
    <br/>
    <br/>
    <form action="" method="POST">
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="active float-left"><a href="/adminonly/companyadmin/$companyid$">Company Information</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/users/$companyid$">Users</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/usagestats/$companyid$">Statistics</a></li>
          <li class="float-left"><a href="/adminonly/companyadmin/payments/$companyid$">Payments</a></li>
          <li class="float-right"/>
	</ul>
	<div class="tab-container">
	  <div class="tab-content account">
	    <table><tbody>
		<tr> <th style="width:200px"></th> <th> </th></tr>
		<tr> <td style="padding-top:10px"><strong>Company data</strong></td> <td> </td></tr>
		<tr> <td>Name:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companyname$"  name="companyname" />
		  </td>
		</tr>
		<tr> <td>Company nr:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companynumber$"  name="companynumber" />
		  </td>
		</tr>
		<tr> <td style="padding-top:10px"><strong>Company Location</strong></td>
		  <td> </td>
		</tr>
		<tr> <td>Adress:</td>
		  <td> <input type="text" style="width:200px;margin:0" value="$companyaddress$"  name="companyaddress" />
		  </td>
		</tr>
		<tr> <td>ZIP:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companyzip$"  name="companyzip" />
		  </td>
		</tr>
		<tr> <td>City:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companycity$"  name="companycity" />
		  </td>
		</tr>
		<tr> <td>Country:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companycountry$" name="companycountry" />
		  </td>
		</tr>
		<tr> <td>IP address mask list:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companyipaddressmasklist$" name="companyipaddressmasklist" />
		  </td>
		</tr>
                $if(hasmailapi)$
		<tr> <td style="padding-top:10px"><strong>Company Mail API</strong></td> <td> </td></tr>
		<tr> <td>emails:</td>
		  <td>

                 <ul id="mailapiemailslist">
                  <li><a href="mailto:contract+$mailapikey$@api.scrive.com">
                    contract+$mailapikey$@api.scrive.com
                   </a></li>
                   <li><a href="mailto:offer+$mailapikey$@api.scrive.com">
                     offer+$mailapikey$@api.scrive.com
                   </a></li>
                   <li><a href="mailto:order+$mailapikey$@api.scrive.com">
                     order+$mailapikey$@api.scrive.com
                   </a></li>
                 </ul>

		  </td>
		</tr>
		<tr> <td>daily limit:</td>
		  <td>
                    $mailapilimit$
                  </td>
                </tr>
		<tr> <td>documents sent today:</td>
		  <td>
                    $mailapisent$
                  </td>
                </tr>
                $endif$
	    </tbody></table>
	    <br/>
            $if(!hasmailapi)$(Clicking "Change" will add MailAPI key for company; new policy is that all companies have it activated by default.) <br />$endif$
	    <input type="submit" style="width:200px;font-weight:bold"
	    value="Change" name="change" />
	  </div>
	</div>
      </div>
    </form>
</div>
</div>
####
admincompanybranding=
<div class="mainContainer" >
  <div class="body-container">
    <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly#companyadmin">Go back to company admin page</a>
    <br/>
    <br/>
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="float-left"><a href="/adminonly/companyadmin/$companyid$">Company Information</a></li>
	  <li class="active float-left"><a href="/adminonly/companyadmin/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/users/$companyid$">Users</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/usagestats/$companyid$">Statistics</a></li>
          <li class="float-left"><a href="/adminonly/companyadmin/payments/$companyid$">Payments</a></li>
          <li class="float-right"/>
	</ul>
      <div class="place-for-branding">
      </div>
    </div>
<script>
var company = new CompanyBranding({ companyid: "$companyid$" });
jQuery('.place-for-branding').append(company.el());
</script>
</div>
</div>
####
admincompanypayments=
<div class="mainContainer" >
  <div class="body-container">
    <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly#companyadmin">Go back to company admin page</a>
    <br/>
    <br/>
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="float-left"><a href="/adminonly/companyadmin/$companyid$">Company Information</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/users/$companyid$">Users</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/usagestats/$companyid$">Statistics</a></li>
          <li class="float-left active"><a href="/adminonly/companyadmin/payments/$companyid$">Payments</a></li>
          <li class="float-right"/>
	</ul>
      <div class="tab-container">
	<div class="tab-content account">
          <form method="POST">
	    <table><tbody>
		<tr> <th style="width:200px"></th> <th> </th></tr>
		<tr> <td style="padding-top:10px"><strong>Payment Info</strong></td> <td> </td></tr>
		<tr>
                  <td>Company Users: </td>
		  <td>
                    $quantity$
		  </td>
		</tr>
                $if(recurlyplan)$
		<tr>
                  <td>Price plan on Recurly: </td>
		  <td>
                    <a href="https://$recurlysubdomain$.recurly.com/accounts/$accountcode$">$priceplan$</a>
		  </td>
		</tr>
		<tr>
                  <td>Status: </td>
		  <td>
                    $status$
		  </td>
		</tr>
                <tr>
                  <td>Migrate: </td>
                  <td>
                    <p>Migrate this user's subscription to another user/company.</p>
                    <p><select name="migratetype"><option value="userid">User</option><option value="companyid">Company</option></select> <input type="text" name="id" placeholder="userid/companyid"></input></p>
                    <p><input type="submit" name="migrate" value="Migrate"></input></p>
                    <p>Be very careful with this! </p>
                  </td>
                </tr>
                <tr>
                  <td>Delete: </td>
                  <td>
                    <p>Delete this subscription. Be careful! This will not delete it from Recurly. Please close the Recurly account first!</p>
                    <p><input type="submit" name="delete" value="Delete"></input></p>
                  </td>
		</tr>
                $else$
		<tr>
                  <td>Price plan (selected by admin):</td>
		  <td>

		    <select name="priceplan">
                      <option value="free"         $if(freeplan)$selected="selected"$endif$>Free (no plan)</option>
                      <option value="team"         $if(teamplan)$selected="selected"$endif$>Team</option>
                      <option value="form"         $if(formplan)$selected="selected"$endif$>Online Forms</option>
                      <option value="enterprise"   $if(enterpriseplan)$selected="selected"$endif$>Enterprise</option>
                      <option value="trial"        $if(trialplan)$selected="selected"$endif$>Trial</option>
                    </select>
		  </td>
		</tr>
		<tr>
                  <td>Status (selected by admin):</td>
		  <td>
		    <select name="status">
                      <option value="active"      $if(activestatus)$selected="selected"$endif$>Active</option>
                      <option value="overdue"     $if(overduestatus)$selected="selected"$endif$>Overdue</option>
                      <option value="canceled"    $if(canceledstatus)$selected="selected"$endif$>Canceled</option>
                      <option value="deactivated" $if(deactivatedstatus)$selected="selected"$endif$>Deactivated</option>
                    </select>
		  </td>
		</tr>
		<tr>
                  <td><input type="submit" style="width:200px;font-weight:bold"
	           value="Change" name="change" /></td>
		  <td>
		  </td>
		</tr>
                $endif$
	    </tbody></table>

          </form>
	</div>
      </div>
    </div>
</div>
</div>
####
admincompanyusers=
<div class="mainContainer" >
  <div class="body-container">
  <a href="/adminonly">Go back to main admin page</a>
  <a style="padding-left:100px" href="/adminonly#companyadmin">Go back to company admin page</a>
  <br/>
  <br/>

  <div class="tab-viewer">
    <div class="tab-header">
    </div>
    <ul class="tabs">
      <li class="float-left"><a href="/adminonly/companyadmin/$companyid$">Company Information</a></li>
      <li class="float-left"><a href="/adminonly/companyadmin/branding/$companyid$">Branding</a></li>
      <li class="active float-left"><a href="/adminonly/companyadmin/users/$companyid$">Users</a></li>
      <li class="float-left"><a href="/adminonly/companyadmin/usagestats/$companyid$">Statistics</a></li>
      <li class="float-left"><a href="/adminonly/companyadmin/payments/$companyid$">Payments</a></li>
      <li class="float-right"/>
    </ul>
  </div>
  <script>

    var newUserInCompanyButton = Button.init({
        color: "green",
        text : "Add new user",
        size : "tiny",
        style: "float:left",
        onClick : function() {
              var body = jQuery("<div class='account-body'>");
              body.append("<p>Create new user account in company</p>");
              var table = jQuery("<table/>");

              var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("First name:"));
              var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
              tr1.append(jQuery("<td/>").append(fstname));
              table.append(tr1);

              var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("Second name:"));
              var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
              tr2.append(jQuery("<td/>").append(sndname));
              table.append(tr2);

              var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("Email address:"));
              var email = jQuery("<input type='text' name='email' autocomplete='off' />");
              tr5.append(jQuery("<td/>").append(email));
              table.append(tr5);

              var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("Language:"));
              var lang = jQuery("<select name='lang'>"
                                  + '<option $if(user.langen)$selected=""$endif$ value="LANG_SV">Sweden (Swedish)</option>'
                                  + '<option $if(user.langsv)$selected=""$endif$ value="LANG_EN">Britain (English)</option>'
                                  + "</select>");
              tr6.append(jQuery("<td/>").append(lang));
              table.append(tr6);

              var tr7 = jQuery("<tr/>").append(jQuery("<td/>").text("Is company admin:"));
              var isadmin = jQuery("<input type='checkbox' name='isadmin' autocomplete='off' />");
              tr7.append(jQuery("<td/>").append(isadmin));
              table.append(tr7);


              var tr8 = jQuery("<tr/>").append(jQuery("<td/>").text("Header:"));
              var btnHeader = jQuery('<input type="button" class="editer" value="Default"/>').click(function(){
                  prepareForEdit(jQuery(this).parents("div"), '300');
                  jQuery(this).hide();
              });
              var header = jQuery('<span class="editable" name="custommessage" style="display: none">'
                                  + jQuery('#message').html()
                                  + '</span>');

              tr8.append(jQuery("<td/>").append(btnHeader).append(header));
              table.append(tr8);

              body.append(table);


              Confirmation.popup({
                onAccept : function() {
                              var callback = function(t,e,v) {
                                  e.css("background", "red");
                                  jQuery(document.createElement('div'))
                                      .attr("name", "validate-message")
                                      .css({"font-size": 8, "font-weight": "bold", "color": "red"})
                                      .append(v.message())
                                      .appendTo(e.parent());
                              };

                              //reset red background
                              fstname.css("background", "white");
                              sndname.css("background", "white");
                              email.css("background", "white");
                              lang.css("background", "white");

                              //delete messages
                              jQuery('[name=validate-message]').remove();

                              var vresult = [
                                  fstname.validate(new NameValidation({callback: callback, message: "Wrong first name format!"})),
                                  sndname.validate(new NameValidation({callback: callback, message: "Wrong second name format!"})),
                                  email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback}))),
                                  lang.validate(new NotEmptyValidation({callback: callback, message:"Language cannot be empty!"}))
                                  ];

                              if (vresult.every(function(a) {return a;})) {
                                  new Submit({
                                      method: "POST",
                                      url: "/adminonly/companyadmin/users/$companyid$",
                                      fstname : fstname.val(),
                                      sndname : sndname.val(),
                                      email : email.val(),
                                      lang : lang.val(),
                                      iscompanyadmin : isadmin.is(":checked") ? "Yes" : undefined,
                                      custommessage : jQuery('textarea',body).val()
                                      }).send();
                              }
                          },
                title : "Create user in company",
                acceptText : "Create",
                content  : body
              });
    } }).input();

    var inviteExistingUserToCompany = Button.init({
      color: "green",
      text : "Invite existing user",
      size : "tiny",
      style: "float:left;margin-left:10px",
      onClick : function() {
      var body = jQuery("<div class='account-body' style='min-height:50px;'>");
              body.append("<p>Create new user account in company</p>");
              var table = jQuery("<table/>");

              var tr = jQuery("<tr/>").append(jQuery("<td/>").text("Email address:"));
              var email = jQuery("<input type='text' name='email' autocomplete='off' />");
              tr.append(jQuery("<td/>").append(email));
              table.append(tr);
              body.append(table);


              Confirmation.popup({
                onAccept : function() {
                              var callback = function(t,e,v) {
                                  e.css("background", "red");
                                  jQuery(document.createElement('div'))
                                      .attr("name", "validate-message")
                                      .css({"font-size": 8, "font-weight": "bold", "color": "red"})
                                      .append(v.message())
                                      .appendTo(e.parent());
                              };

                              email.css("background", "white");


                              //delete messages
                              jQuery('[name=validate-message]').remove();

                              var vresult = [
                                  email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback}))),
                                  ];

                              if (vresult.every(function(a) {return a;})) {
                                  new Submit({
                                      method: "POST",
                                      url: "/adminonly/companyadmin/users/$companyid$",
                                      email : email.val(),
                                      privateinvite : "true",
                                      }).send();
                              }
                          },
                title : "Add existing user to company",
                acceptText : "Add",
                content  : body
              });
      }}).input();


    jQuery(function() {

      var companyAccountsTable = new KontraList({
        name : "CompanyAccountsTable",
        schema: new Schema({
          url: "/adminonly/companyaccounts/$companyid$",
          sorting: new Sorting({ fields: ["fullname",
                                          "email",
                                          "role",
                                          "activated",
                                          "id",
                                          "deletable"] }),
          paging: new Paging({}),
          textfiltering: new TextFiltering({ text: "", infotext: "Company Accounts" }),
          cells: [
            new Cell({name: "Name", width: "100px", field:"fullname", special : "link"}),
            new Cell({name: "Email", width: "100px", field:"email"}),
            new Cell({name: "Role", width: "100px", field:"role", special: "rendered",
                      rendering: function(value, idx, user) {
                        if (user.field("role")=="RoleAdmin") {
                          return jQuery("<span>Admin</span>");
                        } else if (user.field("role")=="RoleStandard") {
                          return jQuery("<span>Standard</span>");
                        } else {
                          return jQuery("<span>Pending</span>");
                        }
                      }})
          ]
        }),
        headerExtras : newUserInCompanyButton.add(inviteExistingUserToCompany)
      });
      jQuery(".tab-viewer").append(companyAccountsTable.el());
    });
  </script>
</div>
</div>
####
userusagestats=
<div class="mainContainer" >
  <div class="body-container">
   <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly/#useradmin">Go back to user admin page</a>
    <br/>
    <br/>
    <div class="tab-viewer">
      <div class="tab-header">
      </div>
      <ul class="tabs">
	<li class="float-left"><a href="/adminonly/useradmin/$user.id$">Användaruppgifter</a></li>
	<li class="float-left"><a href="/adminonly/useradmin/payments/$user.id$">Payment info</a></li>
	<li class="active float-left"><a href="/adminonly/useradmin/usagestats/$user.id$">Statistics</a></li>
    <li class="float-left"><a href="/adminonly/useradmin/documents/$user.id$">Documents</a></li>

        <li class="float-right"/>
      </ul>
      <div class="tab-container">
	<div class="tab-content account">
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">Documents Closed</td>
              <td style="width:200px">Signatures Closed</td>
              <td style="width:200px">Documents Sent</td>
	    </thead>
            $statistics:{stat|<tr>
              <td>$stat.date$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.sent$ </td>
	    </tr>}$
	  </table>
	</div>
      </div>
    </div>
</div>
</div>
####
####
userdocuments=
<div class="mainContainer" >
  <div class="body-container">
   <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly/#useradmin">Go back to user admin page</a>
    <br/>
    <br/>
    <div class="tab-viewer">
      <div class="tab-header">
      </div>
      <ul class="tabs">
    <li class="float-left"><a href="/adminonly/useradmin/$userid$">Användaruppgifter</a></li>
    <li class="float-left"><a href="/adminonly/useradmin/payments/$userid$">Payment info</a></li>
    <li class="float-left"><a href="/adminonly/useradmin/usagestats/$userid$">Statistics</a></li>
    <li class="active float-left"><a href="/adminonly/useradmin/documents/$userid$">Documents</a></li>

        <li class="float-right"/>
      </ul>
      <div class="tab-container">
        <div class="doclist">

        </div>
      </div>
    </div>
</div>
</div>
<script>
  jQuery(".doclist").append(new KontraList(DocumentAdminListDefinition($if(admin)$true$else$false$endif$, "$userid$")).el());
</script>
####
companyusagestats=
<div class="mainContainer" >
  <div class="body-container">
    <a href="/adminonly">Go back to main admin page</a>
    <a style="padding-left:100px" href="/adminonly#companyadmin">Go back to company admin page</a>
    <br/>
    <br/>
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="float-left"><a href="/adminonly/companyadmin/$companyid$">Company Information</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="/adminonly/companyadmin/users/$companyid$">Users</a></li>
	  <li class="active float-left"><a href="/adminonly/companyadmin/usagestats/$companyid$">Statistics</a></li>
          <li class="float-left"><a href="/adminonly/companyadmin/payments/$companyid$">Payments</a></li>
          <li class="float-right" />
	</ul>
      <div class="tab-container">
	<div class="tab-content account">
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">User</td>
              <td style="width:200px">Documents Closed</td>
              <td style="width:200px">Signatures Closed</td>
              <td style="width:200px">Documents Sent</td>
	    </thead>
            $statistics:{stat|<tr class="$if(stat.istotal)$totalline$endif$">
              <td>$stat.date$</td>
              <td>$stat.user$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.sent$ </td>
	    </tr>}$
	  </table>
	</div>
      </div>
    </div>
</div>
</div>
###
daveDocument=
<div class="mainContainer" >
  <div class="body-container">
$daveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
<BR/>
<BR/>
<BR/>
$if(couldBeclosed)$
  <form method="post" action="/dave/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
$elseif(closed)$
  <form method="post" action="/dave/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
$endif$
<BR/>
</div>
</div>
</div>
####
daveSignatoryLink=
<div class="mainContainer" >
  <div class="body-container">
$daveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
This will record the change in the evidence log.
<BR/>
<BR/>
</div>
</div>
</div>
####
