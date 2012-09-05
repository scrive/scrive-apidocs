####
showUser =
<div class="tab-viewer">
  <form action="$linkaccount$" method="post" class="accountForm">
    <div class="tab-header">
    </div>
    <ul class="tabs">
        <li class="active float-left"><a href="/account">$_HeadUserInfo()$</a></li>
        <li class='float-left'><a href="/account/security">$_HeadAccountSettings()$</a></li>
        $if(iscompanyadmin)$<li class='float-left'><a href="/account/company">$_CompanySettings()$</a></li>$endif$
        $if(iscompanyadmin)$<li class='float-left'><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
        <li class='float-left'><a href="/account/mailapi">$_HeadMailApi()$</a></li>
        <li class='float-left'><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
        $if(seessubscriptiondashboard)$<li class="float-left"><a href="/payments/dashboard">$_Subscription()$</a></li>$endif$
        <!--<li><a href="/oauth/dashboard">API</a></li>-->
        <li class='float-right'/>
    </ul>
    <div class="tab-container">
        <div class="tab-content account">
            <div $if(iscompanyaccount)$class="col"$endif$>
                <div class="account-header">
                    <h2>$fstname$ $sndname$</h2>
                    <!-- <a href="javascript:;">Ladda upp ny bild...</a> -->
                    <div class="image">
                        $if(userimagelink)$
                        <img src="$userimagelink$" height="68" width="58" />
                        $endif$
                    </div>
                </div>
                <div class="account-body">
                    <table>
                        <tr>
                            <td>$_firstname()$:</td>
                            <td><input type="text" name="fstname" value="$fstname$" autocomplete="off" /></td>
                        </tr>
                        <tr>
                            <td>$_lastname()$:</td>
                            <td><input type="text" name="sndname" value="$sndname$" autocomplete="off" /></td>
                        </tr>
                        <tr>
                            <td>$_personnumber()$:</td>
                            <td><input type="text" name="personalnumber" value="$personalnumber$" autocomplete="off" /></td>
                        </tr>
                        <tr>
                            <td>$_email()$:</td>
                            <td><input type="email" name="email" value="$email$" autocomplete="off" disabled=""/></td>
                        </tr>
                        <tr>
                            <td>$_phone()$:</td>
                            <td><input type="text" name="phone" value="$phone$" autocomplete="off" /></td>
                        </tr>
                        $if(!iscompanyaccount)$
                        <tr>
                            <td>$_company()$:</td>
                            <td><input type="text" name="companyname" value="$usercompanyname$" autocomplete="off" /></td>
                        </tr>
                        <tr>
                            <td>$_orgnumber()$:</td>
                            <td><input type="text" name="companynumber" value="$usercompanynumber$" autocomplete="off" /></td>
                        </tr>
                        $endif$
                        <tr>
                            <td>$_companyposition()$:</td>
                            <td><input type="text" name="companyposition" value="$companyposition$" autocomplete="off" /></td>
                        </tr>
                    </table>
                </div>
            </div>
            $if(iscompanyaccount)$
            <div class="col">
                <div class="account-header">
                    <h2>$companyname$</h2>
                    <!--<a href="javascript:;">Ladda upp ny logotyp...</a> -->
                    <div class="image">
                        $if(companyimagelink)$
                        <img src="$companyimagelink$" height="68" width="58" />
                        $endif$
                    </div>
                </div>
                <div class="account-body">
                    <table>
                        <tr>
                            <td>$_company()$:</td>
                            <td><input type="text" name="companyname" value="$companyname$" autocomplete="off" $if(!iscompanyadmin)$disabled="disabled"$endif$/></td>
                        </tr>
                        <tr>
                            <td>$_orgnumber()$:</td>
                            <td><input type="text" name="companynumber" value="$companynumber$" autocomplete="off" $if(!iscompanyadmin)$disabled="disabled"$endif$ /></td>
                        </tr>
                        <tr>
                            <td>$_address()$:</td>
                            <td><input type="text" name="companyaddress" value="$address$" autocomplete="off"  $if(!iscompanyadmin)$disabled="disabled"$endif$/></td>
                        </tr>
                        <tr>
                            <td>$_zip()$:</td>
                            <td><input type="email" name="companyzip" value="$zip$" autocomplete="off"  $if(!iscompanyadmin)$disabled="disabled"$endif$/></td>
                        </tr>
                        <tr>
                            <td>$_city()$:</td>
                            <td><input type="text" name="companycity" value="$city$" autocomplete="off"  $if(!iscompanyadmin)$disabled="disabled"$endif$/></td>
                        </tr>
                        <tr>
                            <td>$_country()$:</td>
                            <td><input type="text" name="companycountry" value="$country$" autocomplete="off"  $if(!iscompanyadmin)$disabled="disabled"$endif$/></td>
                        </tr>
                    </table>
                </div>
            </div>
            $endif$
            <input type="submit" class="hidden" />
            <div class="account-footer">
                  $if(!iscompanyaccount)$
                  <span class="createcompanybuttonbox"></span>
                  $endif$
                  <a href="#" class="btn-small green submit s-submit-user-settings">
                    <div class="left"></div>
                    <div class="label">
                        <p>$_SaveChanges()$</p>
                    </div>
                    <div class="right"></div>
                  </a>
            </div>
            <div class="clearfix"></div>

        </div>
    </div>
  </form>
</div>
<script>
  jQuery(function() {
    var changeEmailRequestButton = Button.init({
    color: "green",
    size: "tiny",
    text: "$_changeEmailRequestButtonText()$",
    onClick: function() {
        var body = jQuery("<div>");
        var p = jQuery("<p>").text("$_changeEmailRequestExplanation()$");
        body.append(p);
        var table = jQuery("<table/>");

        var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("$_newemail()$:"));
        var newemail = jQuery("<input type='text' name='newemail' autocomplete='off' />");
        tr1.append(jQuery("<td/>").append(newemail));
        table.append(tr1);

        var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("$_newemailagain()$:"));
        var newemailagain = jQuery("<input type='text' name='newemailagain' autocomplete='off' />");
        tr2.append(jQuery("<td/>").append(newemailagain));
        table.append(tr2);

        body.append(table);


        Confirmation.popup({
          onAccept: function() {
                      new Submit({
                        method: "POST",
                        changeemail: "true",
                        newemail: newemail.val(),
                        newemailagain: newemailagain.val(),
                        fstname: jQuery("input[name='fstname']").val(),
                        sndname: jQuery("input[name='sndname']").val(),
                        personalnumber: jQuery("input[name='personalnumber']").val(),
                        phone: jQuery("input[name='phone']").val(),
                        companyposition: jQuery("input[name='companyposition']").val(),
                        companynumber: jQuery("input[name='companynumber']").val(),
                        companyname: jQuery("input[name='companyname']").val(),
                        orgnumber: jQuery("input[name='orgnumber']").val(),
                        companyaddress: jQuery("input[name='companyaddress']").val(),
                        companyzip: jQuery("input[name='companyzip']").val(),
                        copanycity: jQuery("input[name='copanycity']").val(),
                        companycountry: jQuery("input[name='companycountry']").val()
                        }).send();
                      },
          title: "$_changeEmailRequestTitle()$",
          acceptButtonText: "$_changeEmailRequestAcceptText()$",
          content: body
        });
        return false;
      }
    });

    jQuery('[name="email"]').parent().append(changeEmailRequestButton.input());

    var addCompanyAccountButton = Button.init({
    color: "green",
    size: "small",
    text: "$_createCompany()$",
    onClick: function() {
        var body = jQuery("<div class='account-body'>");
        var firstp = jQuery("<p>").text("$_createCompanyExplanation()$");
        var secondp = jQuery("<p>").text("$_createCompanyFormPreamble()$");
        body.append(firstp);
        body.append(secondp);
        var table = jQuery("<table/>");

        var company = jQuery('input[name=companyname]').val();
        var orgnum = jQuery('input[name=companynumber]').val();

        var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("$_company()$:"));
        var companyname = jQuery("<input type='text' name='companyname' autocomplete='off' />");
        tr1.append(jQuery("<td/>").append(companyname));
        table.append(tr1);
        companyname.val(company);

        var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("$_orgnumber()$:"));
        var companynumber = jQuery("<input type='text' name='companynumber' autocomplete='off' />");
        tr2.append(jQuery("<td/>").append(companynumber));
        table.append(tr2);
        companynumber.val(orgnum);

        var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("$_address()$"));
        var companyaddress = jQuery("<input type='text' name='companyaddress' autocomplete='off' />");
        tr5.append(jQuery("<td/>").append(companyaddress));
        table.append(tr5);

        var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("$_zip()$"));
        var companyzip = jQuery("<input type='text' name='companyzip' autocomplete='off' />");
        tr6.append(jQuery("<td/>").append(companyzip));
        table.append(tr6);

        var tr7 = jQuery("<tr/>").append(jQuery("<td/>").text("$_city()$"));
        var companycity = jQuery("<input type='text' name='companycity' autocomplete='off' />");
        tr7.append(jQuery("<td/>").append(companycity));
        table.append(tr7);

        var tr8 = jQuery("<tr/>").append(jQuery("<td/>").text("$_country()$"));
        var companycountry = jQuery("<input type='text' name='companycountry' autocomplete='off' />");
        tr8.append(jQuery("<td/>").append(companycountry));
        table.append(tr8);

        body.append(table);


        Confirmation.popup({
          onAccept: function() {
            var callback = function(t,e,v) {
                e.css("background", "red");
                jQuery(document.createElement('div'))
                    .attr("name", "validate-message")
                    .css({"font-size": 8, "font-weight": "bold", "color": "red"})
                    .append(v.message())
                    .appendTo(e.parent());
            };

            //reset red background
            companyname.css("background", "white");

            //delete messages
            jQuery('[name=validate-message]').remove();

            var vresult = [
                companyname.validate(new NotEmptyValidation({callback: callback, message: "Company name cannot be empty!"}))
                ];

            if (vresult.every(function(a) {return a;})) {
              new Submit({
                method: "POST",
                createcompany: "true",
                companyname: companyname.val(),
                companynumber: companynumber.val(),
                companyaddress: companyaddress.val(),
                companyzip: companyzip.val(),
                companycity: companycity.val(),
                companycountry: companycountry.val(),
                fstname: jQuery("input[name='fstname']").val(),
                sndname: jQuery("input[name='sndname']").val(),
                personalnumber: jQuery("input[name='personalnumber']").val(),
                phone: jQuery("input[name='phone']").val(),
                companyposition: jQuery("input[name='companyposition']").val()
                }).send();
              return false;
            }
          },
          title: "$_createCompany()$",
          acceptButtonText: "$_createCompany()$",
          content: body
        });
        return false;
      }
    });

  $if(!iscompanyaccount)$
      jQuery(".createcompanybuttonbox").append(addCompanyAccountButton.input());
  $endif$
  });
</script>
#####
