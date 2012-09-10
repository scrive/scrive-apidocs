###
adminsmain=
<strong>Stats CSVs</strong>
<p><a href="/adminonly/allstatscsv">Download the CSV containing  all signature stats</a></p>
<p><a href="/adminonly/userstatscsv">Download the CSV containing all
    user stats</a></p>
<p><a href="/adminonly/signstatscsv">Download the CSV containing all
    sign stats</a></p>
<p><a href="/adminonly/dochistorycsv">Download the CSV containing
    document histories</a></p>
<p><a href="/adminonly/signhistorycsv">Download the CSV containing
    sign histories</a></p>
<p><a href="/adminonly/userslistcsv">Download the CSV containing all
    users (not suspended, with accepted TOS, no integration parties)</a></p>
    <strong>Sales Tools</strong>
    <table><tbody>
        <tr>
          <td style="height:50px;width:200px">
            <a href="/adminonly/useradminforsales">
              <button style="width:150px"> Sales user admin</button>
            </a>
          </td>
          <td style="height:50px;width:200px">
            <a href="/adminonly/companyadmin">
              <button style="width:150px"> Company Admin </button>
            </a>
          </td>
       </tr>
        <tr>
          <td style="height:50px;width:200px">
            <a href="/adminonly/statistics">
              <button style="width:150px"> Statistics</button>
            </a>
          </td>
          <td style="height:50px;">
            <a href="/adminonly/functionalitystats">
              <button style="width:150px">Functionality Stats</button>
            </a>
          </td>
        </tr>
        <tr>
          <td style="height:50px">
            <a href="/adminonly/documents">
              <button style="width:150px">Documents</button>
            </a>
          </td>
          <td style="height:50px;width:200px"> <a href="/adminonly/useradmin"><button style="width:150px"> User Admin </button> </a></td>
        </tr>
    </tbody></table>
    <br/>
$if(admin)$
    <strong> Admin Tools  </strong>
    <table><tbody>
        <tr>
          <td style="height:50px;width:200px"><a href="/adminonly/services"><button style="width:150px"> Services </button> </a> </td>
        </tr>
        <tr>
        </tr>
    </tbody></table>

    <br/>
    <br/>
    <strong> Log Files </strong>
    <ul>
      <li><a href="/adminonly/log/debug.log">debug.log</a></li>
      <li><a href="/adminonly/log/mail.log">mail.log</a></li>
      <li><a href="/adminonly/log/error.log">error.log</a></li>
    </ul>
$endif$

#####
adminusers=
   <div class="tab-viewer">
         <div class="tab-header">
          <strong>
           <span>USER ADMIN</span>
           <span style="margin-left:20px">  $backtoadminmainlink()$</span>
          </strong>
          </div>
       <div id="adminusers-viewer"></div>
    </div>
    <script>
    var adminUsersTable = KontraList().init({
        name : "Admin Users Table",
        schema: new Schema({
            url: "/adminonly/userslist",
            sorting: new Sorting({
                fields: [
                      "username"
                    , "email"
                    , "company"
                    , "uploaded_docs"
                    , "signed_docs"
                    , "signed_1m"
                    , "signed_2m"
                    , "signed_3m"
                    , "signed_6m"
                    , "signed_12m"
                    ]}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({text: "", infotext: "Username, email or company name."}),
            cells : [
                new Cell({name: "Username"     , width:"100px", field:"username", special:"link"}),
                new Cell({name: "Email"        , width:"140px", field:"email", special:"link"}),
                new Cell({name: "Company"      , width:"120px", field:"company", special:"link"}),
                new Cell({name: "Uploaded Docs", width:"50px",  field:"uploaded_docs", special:"link"}),
                new Cell({name: "Signed (tot)" , width:"40px",  field:"signed_docs", special:"link"}),
                new Cell({name: "Signed (1m)"  , width:"40px",  field:"signed_1m", special:"link"}),
                new Cell({name: "Signed (2m)"  , width:"40px",  field:"signed_2m", special:"link"}),
                new Cell({name: "Signed (3m)"  , width:"40px",  field:"signed_3m", special:"link"}),
                new Cell({name: "Signed (6m)"  , width:"40px",  field:"signed_6m", special:"link"}),
                new Cell({name: "Signed (12m)" , width:"40px",  field:"signed_12m", special:"link"}),
                new Cell({name: "Viral Invites", width:"40px",  field:"viral_invites", special:"bool"}),
                new Cell({name: "Admin Invites", width:"40px",  field:"admin_invites", special:"bool"})
                ]
        })
    });

    jQuery(function(){
        jQuery("#adminusers-viewer").append(adminUsersTable.view.el)
    });
    </script>

###
admincompanies=
   <div class="tab-viewer">
         <div class="tab-header">
          <strong>
           <span>COMPANY ADMIN</span>
           <span style="margin-left:20px">  $backtoadminmainlink()$</span>
          </strong>
          </div>
       <div id="companies-viewer"></div>
    </div>
<script>
  var companiesTable = KontraList().init({
    name : "CompaniesTable",
    schema: new Schema({
      url: "/adminonly/companies",
      sorting: new Sorting({ fields: [
                                       "companyname",
                                       "companynumber",
                                       "companyaddress",
                                       "companyzip",
                                       "companycity",
                                       "companycountry"  ] }),
      paging: new Paging({}),
      textfiltering: new TextFiltering({ text: "", infotext: "Search companies" }),
      cells : [
        new Cell({name: "Name",   width:"120px", field:"companyname"   , special:"link"}),
        new Cell({name: "Number", width:"100px", field:"companynumber" , special:"link"}),
        new Cell({name: "Address",width:"200px", field:"companyaddress", special:"link"}),
        new Cell({name: "Zip",    width:"50px",  field:"companyzip"    , special:"link"}),
        new Cell({name: "City",   width:"100px", field:"companycity"   , special:"link"}),
        new Cell({name: "Contry", width:"100px", field:"companycountry", special:"link"}),
        new Cell({name: "ID",     width:"120px", field:"id"            , special:"link"})
      ]
    })
  });

  jQuery(function(){
    jQuery("#companies-viewer").append(companiesTable.view.el);
  });
</script>
###
adminUsersForSales=
   <div class="tab-viewer">
         <div class="tab-header">
          <strong>
           <span>USER ADMIN FOR SALES</span>
           <span style="margin-left:20px">  $backtoadminmainlink()$</span>
          </strong>
          </div>
       <div id="users-viewer"></div>
    </div>
    <div id="message" style="display: none">$mailNewAccountCreatedByAdminContentHeader()$</div>
    <script>
    var createUserBtn = Button.init({
        color: "green",
        size: "tiny",
        text: "Create user",
        onClick: function() {
            var body = jQuery("<div class='account-body'>");
            body.append("<p>Create new user account</p>");
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

            var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("Region:"));
            var region = jQuery("<select name='region'>"
                                + '<option $if(user.regiongb)$selected=""$endif$ value="REGION_SE">Sweden (Swedish)</option>'
                                + '<option $if(user.regionse)$selected=""$endif$ value="REGION_GB">Britain (English)</option>'
                                + "</select>");
            tr6.append(jQuery("<td/>").append(region));
            table.append(tr6);

            var tr7 = jQuery("<tr/>").append(jQuery("<td/>").text("Header:"));
            var btnHeader = jQuery('<input type="button" class="editer" value="Default"/>').click(function(){
                prepareForEdit(jQuery(this).parents("div"), '300');
                jQuery(this).hide();
            });
            var header = jQuery('<span class="editable" name="custommessage" style="display: none">'
                                + jQuery('#message').html()
                                + '</span>');

            tr7.append(jQuery("<td/>").append(btnHeader).append(header));
            table.append(tr7);

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
                            region.css("background", "white");

                            //delete messages
                            jQuery('[name=validate-message]').remove();

                            var vresult = [
                                fstname.validate(new NameValidation({callback: callback, message: "Wrong first name format!"})),
                                sndname.validate(new NameValidation({callback: callback, message: "Wrong second name format!"})),
                                email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback}))),
                                region.validate(new NotEmptyValidation({callback: callback, message:"Region cannot be empty!"}))
                                ];

                            if (vresult.every(function(a) {return a;})) {
                                new Submit({
                                    method: "POST",
                                    add : "true",
                                    url: "/createuser",
                                    fstname : fstname.val(),
                                    sndname : sndname.val(),
                                    email : email.val(),
                                    region : region.val(),
                                    custommessage : jQuery('textarea',body).val()
                                    }).send();
                            }
                         },
              title : "Create user",
              acceptButtonText : "OK",
              content  : body
            });
        }
    });

    var usersTable = KontraList().init({
        name : "Users Table",
        schema: new Schema({
            url: "/adminonly/userslist",
            sorting: new Sorting({ fields: ["username", "email", "company", "phone", "tos", "signed_docs"]}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({text: "", infotext: "Username, email or company name."}),
            cells : [
                new Cell({name: "Username", width:"130px", field:"username", special:"link"}),
                new Cell({name: "Email", width:"130px", field:"email", special:"link"}),
                new Cell({name: "Company", width:"100px", field:"company", special:"link"}),
                new Cell({name: "Phone", width:"100px", field:"phone", special:"link"}),
                new Cell({name: "TOS date", width:"100px", field:"tos", special:"link"}),
                new Cell({name: "Signed Docs", width:"100px", field:"signed_docs", special:"link"}),
                new Cell({name: "Subaccounts", width:"100px", field:"subaccounts", special:"link"})
                ]
        }),
        headerExtras: createUserBtn.input()
    });

    jQuery(function(){
        jQuery("#users-viewer").append(usersTable.view.el)
    });
    </script>
###
adminuser=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$adminuserslink$">Go back to user admin page</a>
    <br/>
    <br/>
    <form action="" method="POST">
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="active float-left"><a href="$adminuserslink$/$user.id$">Användaruppgifter</a></li>
	  <li class="float-left"><a href="#">Payment info</a></li>
	  <li class="float-left"><a href="#">Payment list</a></li>
	  <li class="float-left"><a href="$adminuserslink$/usagestats/$user.id$">Statistics</a></li>
          <li class="float-right"/>
	</ul>
	<div class="tab-container">
	  <div class="tab-content account">
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
		<tr> <td>Mobile:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.mobile$" name="usermobile" />
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
    <tr> <td>Region:</td>
      <td><select name="userregion">
        <option $if(user.regionse)$selected=""$endif$ value="REGION_SE">Sweden (Swedish)</option>
        <option $if(user.regiongb)$selected=""$endif$ value="REGION_GB">Britain (English)</option>
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
	    </tbody></table>
	    <br/>
	    <input type="submit" style="width:200px;font-weight:bold"
	    value="Change" name="change" />
	  </div>
	</div>
      </div>
    </form>
####
admincompany=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$admincompanieslink$">Go back to company admin page</a>
    <br/>
    <br/>
    <form action="" method="POST">
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="active float-left"><a href="$admincompanieslink$/$companyid$">Company Information</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/users/$companyid$">Users</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/usagestats/$companyid$">Statistics</a></li>
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
		<tr> <td style="padding-top:10px"><strong>Company email domain</strong></td> <td>optional; be careful with this; example input: <b>scrive.com</b></td></tr>
		<tr> <td>Email domain:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companyemaildomain$" name="companyemaildomain" />
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
####
admincompanybranding=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$admincompanieslink$">Go back to company admin page</a>
    <br/>
    <br/>
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="float-left"><a href="$admincompanieslink$/$companyid$">Company Information</a></li>
	  <li class="active float-left"><a href="$admincompanieslink$/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/users/$companyid$">Users</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/usagestats/$companyid$">Statistics</a></li>
          <li class="float-right"/>
	</ul>
      <div class="tab-container">
	<div class="tab-content account">
	</div>
      </div>
    </div>
<script>
var company = CompanyBranding.init({ companyid: "$companyid$" });
var tabcontent = jQuery(".tab-content");
tabcontent.append(company.input());
tabcontent.append(jQuery("<div class='clearfix' />"));
</script>
####
admincompanyusers=
<a href="$adminlink$">Go back to main admin page</a>
<a style="padding-left:100px" href="$admincompanieslink$">Go back to company admin page</a>
<br/>
<br/>

<div class="tab-viewer">
  <div class="tab-header">
  </div>
  <ul class="tabs">
    <li class="float-left"><a href="$admincompanieslink$/$companyid$">Company Information</a></li>
    <li class="float-left"><a href="$admincompanieslink$/branding/$companyid$">Branding</a></li>
    <li class="active float-left"><a href="$admincompanieslink$/users/$companyid$">Users</a></li>
    <li class="float-left"><a href="$admincompanieslink$/usagestats/$companyid$">Statistics</a></li>
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

            var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("Region:"));
            var region = jQuery("<select name='region'>"
                                + '<option $if(user.regiongb)$selected=""$endif$ value="REGION_SE">Sweden (Swedish)</option>'
                                + '<option $if(user.regionse)$selected=""$endif$ value="REGION_GB">Britain (English)</option>'
                                + "</select>");
            tr6.append(jQuery("<td/>").append(region));
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
                            region.css("background", "white");

                            //delete messages
                            jQuery('[name=validate-message]').remove();

                            var vresult = [
                                fstname.validate(new NameValidation({callback: callback, message: "Wrong first name format!"})),
                                sndname.validate(new NameValidation({callback: callback, message: "Wrong second name format!"})),
                                email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback}))),
                                region.validate(new NotEmptyValidation({callback: callback, message:"Region cannot be empty!"}))
                                ];

                            if (vresult.every(function(a) {return a;})) {
                                new Submit({
                                    method: "POST",
                                    url: "$admincompanieslink$/users/$companyid$",
                                    fstname : fstname.val(),
                                    sndname : sndname.val(),
                                    email : email.val(),
                                    region : region.val(),
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
                                    url: "$admincompanieslink$/users/$companyid$",
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

    var companyAccountsTable = KontraList().init({
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
    jQuery(".tab-viewer").append(companyAccountsTable.view.el);
  });
</script>
####
userusagestats=
   <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$adminuserslink$">Go back to user admin page</a>
    <br/>
    <br/>
    <div class="tab-viewer">
      <div class="tab-header">
      </div>
      <ul class="tabs">
	<li class="float-left"><a href="$adminuserslink$/$user.id$">Användaruppgifter</a></li>
	<li class="float-left"><a href="#">Payment info</a></li>
	<li class="float-left"><a href="#">Payment list</a></li>
	<li class="active float-left"><a href="$adminuserslink$/usagestats/$user.id$">Statistics</a></li>
        <li class="float-right"/>
      </ul>
      <div class="tab-container">
	<div class="tab-content account">
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">Documents Closed That Day</td>
              <td style="width:200px">Total Closed Signatures That Day</td>
              <td style="width:200px">Average Signatures / Doc</td>
              <td style="width:200px">Documents Sent That Day</td>
	    </thead>
            $statistics:{stat|<tr>
              <td>$stat.date$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.avg$ </td>
              <td>$stat.sent$ </td>
	    </tr>}$
	  </table>
	</div>
      </div>
    </div>
####
companyusagestats=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$admincompanieslink$">Go back to company admin page</a>
    <br/>
    <br/>
      <div class="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="float-left"><a href="$admincompanieslink$/$companyid$">Company Information</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/branding/$companyid$">Branding</a></li>
	  <li class="float-left"><a href="$admincompanieslink$/users/$companyid$">Users</a></li>
	  <li class="active float-left"><a href="$admincompanieslink$/usagestats/$companyid$">Statistics</a></li>
          <li class="float-right" />
	</ul>
      <div class="tab-container">
	<div class="tab-content account">
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">User</td>
              <td style="width:200px">Documents Closed</td>
              <td style="width:200px">Total Closed Signatures</td>
              <td style="width:200px">Average Signatures / Doc</td>
              <td style="width:200px">Documents Sent</td>
	    </thead>
            $statistics:{stat|<tr class="$if(stat.istotal)$totalline$endif$">
              <td>$stat.date$</td>
              <td>$stat.user$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.avg$ </td>
              <td>$stat.sent$ </td>
	    </tr>}$
	  </table>
	</div>
      </div>
    </div>
###
statisticsPage=
    $backtoadminmainlink()$
    <div>
      <h1>Stats this month by date</h1>
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">Documents Closed</td>
              <td style="width:200px">Total Closed Signatures</td>
              <td style="width:200px">Average Signatures / Doc</td>
              <td style="width:200px">Documents Sent</td>
              <td style="width:200px">New Users (TOS)</td>
	    </thead>
            $statisticsbyday:{stat|<tr>
              <td>$stat.date$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.avg$ </td>
              <td>$stat.sent$ </td>
              <td>$stat.users$ </td>
	    </tr>}$
	  </table>
      <h1>Stats by month</h1>
	  <table>
            <thead>
              <td style="width:200px">Month</td>
              <td style="width:200px">Documents Closed</td>
              <td style="width:200px">Total Closed Signatures</td>
              <td style="width:200px">Average Signatures / Doc</td>
              <td style="width:200px">Documents Sent</td>
              <td style="width:200px">New Users (TOS)</td>
	    </thead>
            $statisticsbymonth:{stat|<tr>
              <td>$stat.date$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.avg$ </td>
              <td>$stat.sent$ </td>
              <td>$stat.users$ </td>
	    </tr>}$
	  </table>
    </div>
###
###
adminFunctionalityStatsPage=
    $backtoadminmainlink()$
    <div>
      <h1>Functionality Statistics</h1><br/><br/>
      <p>The purpose of these stats is to indicate how much functionality is being used.  Deleted documents or users are not counted here.</p><br/><br/>
      <p>A recent user is one that accepted the TOS within the last 90 days.  A recent document is one created within the last 90 days.</p><br/><br/>
	    <table>
        <thead>
          <th style="width:400px;text-align:right;padding-right:20px;"></th>
          <th style="width:200px;text-align:left;">Recent User Count</th>
	      </thead>
        $userfunctionalitystats:{stat|
        <tr>
          <th style="text-align:right;padding-right:20px;">$stat.label$</th>
          <td>$stat.count$ </td>
	      </tr>
	      }$
	    </table><br/><br/>
	    <table>
        <thead>
          <th style="width:400px;text-align:right;padding-right:20px;"></th>
          <th style="width:200px;text-align:left;">Recent Document Count</th>
	      </thead>
        $docfunctionalitystats:{stat|
        <tr>
          <th style="text-align:right;padding-right:20px;">$stat.label$</th>
          <td>$stat.count$ </td>
	      </tr>
	      }$
	    </table><br/><br/>
	    <table>
	    </table>
    </div>
###
backtoadminmainlink=
   <a href="$adminlink$">Go back to main admin page</a>
    <br/><br/>
####
admindocumentslist =

   <div class="tab-viewer">
         <div class="tab-header">
          <strong>
           <span>DOCUMENTS</span>
           <span style="margin-left:20px">  $backtoadminmainlink()$</span>
          </strong>
          </div>
         <div id="documents-viewer"></div>
    <script>
    var docTable = KontraList().init({
        name : "Documents Table",
        timeout : 20000,
        schema: new Schema({
            url: "/adminonly/documentslist",
            sorting: new Sorting({
                fields: [
                      "ctime"
                    , "mtime"
                    , "id"
                    , "author"
                    , "title"
                    , "service"
                    , "status"
                    , "type"
                    , "signs"
                    ]}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({infotext: "Filter documents"}),
            cells : [
                new Cell({name: "Dates", width:"80px", field: "ctime", special: "rendered",
                        rendering: function(_, _, doc) {
                               return jQuery("<small/>")
                                           .append(jQuery("<span/>").text(doc.field("ctime")))
                                           .append("<BR/>")
                                           .append(jQuery("<span/>").text(doc.field("mtime")));
                          }}),
                new Cell({name: "Author", width:"120px", field:"author", special: "rendered",
                        rendering: function(value, idx, doc) {
                            if (value != undefined)
                                return jQuery("<small/>")
                                                  .append(jQuery("<span/>").text(value.name))
                                                  .append("<BR/>")
                                                  .append(jQuery("<span/>").text(value.email)) 
                                                  .append("<BR/>")
                                                  .append(jQuery("<span/>").text(value.company));
                          }}),
                new Cell({name: "Title", width:"120px", field:"title",  special: "rendered",
                        rendering: function(value, idx, doc) {
                                return jQuery("<small/>").text(value);
                          }}),
                new Cell({name: "Service", width:"40px", field:"service", special: "rendered",
                          rendering: function(value, idx, doc) {
                            if (value == undefined)
                                return jQuery("<small/>").text(value);

                            return jQuery("<small>Scrive</small>");
                          }}),
                new Cell({name: "Status", width:"40px", field:"status",  special: "rendered",
                        rendering: function(value, idx, doc) {
                                return jQuery("<small/>").text(value);
                          }}),
                new Cell({name: "Type", width:"40px", field:"type",  special: "rendered",
                        rendering: function(value, idx, doc) {
                                return jQuery("<small/>").text(value);
                          }}),
                new Cell({name: "Signatories", width:"120px", field:"signs", special: "rendered",
                          rendering: function(value, idx, doc) {
                            if (value == undefined)
                                return ;
                            var res = jQuery("<small/>");
                            _.each(value,function(v) {res.append(jQuery("<span/>").text(v)).append("<BR/>");});
                            return res;
                          }}),
                $if(admin)$
                new Cell({name: "DAVE", width:"30px", field:"id", special:"rendered",
                          rendering: function(value, idx, doc) {
                            return jQuery("<a class='gotodave'>").attr('href','/dave/document/' + value + '/');
                          }})
                $endif$

                ]
        })
    });

    jQuery(function(){
        jQuery("#documents-viewer").append(docTable.view.el)
    });
    </script>
    </div>

####
daveDocument=
$daveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
<BR/>
<BR/>
<BR/>
$if(couldBeclosed)$
  <form method="post" action="/adminonly/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
$elseif(closed)$
  <form method="post" action="/adminonly/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
  <form method="post" action="/adminonly/replacemainfile/$id$" enctype="multipart/form-data">
    <input type="file" name="file"> </input> <input type="submit" value="Reupload sealed file"></input>
  </form>
$else$
  <form method="post" action="/adminonly/replacemainfile/$id$" enctype="multipart/form-data">
    <input type="file" name="file"> </input> <input type="submit" value="Reupload main file"/>
  </form>
$endif$
<BR/>
</div>
####
daveSignatoryLink=
$daveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
This will record the change in the evidence log.
<BR/>
<BR/>
<BR/>
<BR/>
$fields:{field | <form action="/adminonly/updatefields/$docid$/$slid$" method="post">$field.label$: <input type="hidden" name="fieldname" value="$field.fieldname$" /><input type="text" name="fieldvalue" value="$field.fieldvalue$" /><input type="submit" /></form> }$
</div>
####
