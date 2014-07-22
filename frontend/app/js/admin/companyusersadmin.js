/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

define(['Backbone', 'legacy_code'], function() {

window.CompanyUsersList = function(args) {
  var schema, list;

  var newUserInCompanyButton = function(companyid) {
    return new Button({
      color: "green",
      text : "Add new user in company",
      size : "tiny",
      style: "float:left",
      onClick : function() {
        var body = jQuery("<div class='standard-input-table'>");
        body.append("<p>Create new user account in company</p>");
        var table = jQuery("<table/>");

        var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("First name:"));
        var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
        tr1.append(jQuery("<td/>").append(fstname));
        var tr1ErrorRow = $('<tr class="validation-tr"><td/><td><div class="validation-failed-msg">Wrong first name format!</div></td></tr>');
        tr1ErrorRow.css('display', 'none');
        table.append(tr1).append(tr1ErrorRow);

        var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("Second name:"));
        var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
        tr2.append(jQuery("<td/>").append(sndname));
        var tr2ErrorRow = $('<tr class="validation-tr"><td/><td><div class="validation-failed-msg">Wrong second name format!</div></td></tr>');
        tr2ErrorRow.css('display', 'none');
        table.append(tr2).append(tr2ErrorRow);

        var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("Email address:"));
        var email = jQuery("<input type='text' name='email' autocomplete='off' />");
        tr5.append(jQuery("<td/>").append(email));
        var tr5ErrorRow = $('<tr class="validation-tr"><td/><td><div class="validation-failed-msg">Wrong email format!</div></td></tr>');
        tr5ErrorRow.css('display', 'none');
        table.append(tr5).append(tr5ErrorRow);

        var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("Language:"));
        var lang = jQuery("<select name='lang'>"
                          + '<option value="sv">Swedish</option>'
                          + '<option value="en">English</option>'
                          + '<option value="de">German</option>'
                          + '<option value="fr">French</option>'
                          + '<option value="nl">Dutch</option>'
                          + '<option value="it">Italian</option>'
                          + '<option value="no">Norwegian</option>'
                          + '<option value="pt">Portuguese</option>'
                          + '<option value="es">Spanish</option>'
                          + '<option value="da">Danish</option>'
                          + '<option value="el">Greek</option>'
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


        var confirmation = new Confirmation({
          onAccept : function() {
            // Hide validation rows
            tr1ErrorRow.css('display', 'none');
            tr2ErrorRow.css('display', 'none');
            tr5ErrorRow.css('display', 'none');

            // Validate fields
            var validationResult = true;
            if(!new NameValidation().validateData(fstname.val())) {
              tr1ErrorRow.css('display','');
              validationResult = false;
            }
            if(!new NameValidation().validateData(sndname.val())) {
              tr2ErrorRow.css('display','');
              validationResult = false;
            }
            if(!new NotEmptyValidation().validateData(email.val())
               ||
               !new EmailValidation().validateData(email.val())
              ) {
              tr5ErrorRow.css('display','');
              validationResult = false;
            }

            if (validationResult) {
              var successCallback = function(resp) {
                resp = JSON.parse(resp);
                if (resp.success) {
                  confirmation.close();
                  list.recall();
                } else {
                  new FlashMessage({color: 'red', content: resp.error_message});
                }
              };
              new Submit({
                method: "POST",
                url: "/adminonly/companyadmin/users/" + companyid,
                fstname : fstname.val(),
                sndname : sndname.val(),
                email : email.val(),
                lang : lang.val(),
                iscompanyadmin : isadmin.is(":checked") ? "Yes" : undefined,
                custommessage : jQuery('textarea',body).val()
              }).sendAjax(successCallback, function() {window.location.reload();});
            }
          },
          title : "Create user in company",
          acceptText : "Create",
          content  : body
        });
      } }).el();
  };

  schema = {
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
        new Cell({name: "Email", width: "100px", field:"email", special : "link"}),
        new Cell({name: "Role", width: "100px", field:"role", special: "rendered",
                  rendering: function(value, idx, user) {
                    if (user.field("role")=="RoleAdmin") {
                      return jQuery("<span>Admin</span>");
                    } else if (user.field("role")=="RoleStandard") {
                      return jQuery("<span>Standard</span>");
                    } else if (user.field("role")=="RoleInvite") {
                      return jQuery("<span>!User in different company!</span>");
                    }
                  }}),
        new Cell({name: "TOS date", width:"100px", field:"id", special:"rendered",
                  rendering: function(_, idx, user) {
                    var time = user.field("tos");
                    if (user.field("role") == "RoleInvite") {
                      return $("<a style='color:red;' href='#'> Click to delete this invitation </a>").click(
                        function() {
                          new Confirmation({
                            title : "Delete invitation",
                            acceptText: "Delete",
                            content : $("<div style='text-align:center;'>Are you sure that you want to delete this invitation?<BR/>"
                                        + " User will still exist in his original company.</div>"),
                            onAccept : function() {
                              new Submit({
                                url: "/adminonly/useradmin/deleteinvite/" + args.companyid + "/" +user.field("id"),
                                method: "POST",
                                ajaxsuccess: function() {
                                  window.location.reload();
                                  return false;
                                },
                                ajaxerror: function() {
                                  new FlashMessage({color: "red", content : "Failed"});
                                  return false;
                                }}).sendAjax();
                            }
                          });
                        });
                    } else if (time != undefined && time != "") {
                      return $("<small/>").text(new Date(Date.parse(time)).toTimeAbrev());
                    } else return $("<small/>");
                  }
                 })

      ]
    }),
    headerExtras : newUserInCompanyButton(args.companyid)
  };
  list = new KontraList(schema);
  return list;
};
});
