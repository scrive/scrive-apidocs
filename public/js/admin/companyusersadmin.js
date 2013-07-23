/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

(function(window){

var newUserInCompanyButton = function(companyid) {
      return new Button({
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
                                      url: "/adminonly/companyadmin/users/" + companyid,
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
    } }).el();
};

var inviteExistingUserToCompany = function(companyid) {
      return new Button({
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
                                    email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback})))
                                    ];

                                if (vresult.every(function(a) {return a;})) {
                                    new Submit({
                                        method: "POST",
                                        url: "/adminonly/companyadmin/users/" + companyid,
                                        email : email.val(),
                                        privateinvite : "true"
                                        }).send();
                                }
                            },
                  title : "Add existing user to company",
                  acceptText : "Add",
                  content  : body
                });
        }}).el();
};

window.CompanyUsersListDefinition = function(args) {
    return {
        name : "CompanyAccountsTable",
        schema: new Schema({
          url: "/adminonly/companyaccounts/" + args.companyid,
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
        headerExtras : newUserInCompanyButton(args.companyid).add(inviteExistingUserToCompany(args.companyid))
    };
};


})(window);
