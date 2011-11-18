####    Pages for users - usual from my account
viewCompanyAccounts =
<div>
  <div id="tab-viewer">
      <div class="tab-header"></div>
      <ul class="tabs">
        <li><a href="/account">$_HeadUserInfo()$</a></li>
        <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        <li class="active"><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>
        <li><a href="/account/sharing">$_sharing()$</a></li>
        <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      </ul>
  </div>
</div>

<script>
 jQuery(function(){
  var addCompanyButton = Button.init({
        color: "green",
        size: "tiny",
        text: "$_newCompanyAccountButtonText()$",
        onClick: function() {
            var body = jQuery("<div class='account-body'>");
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("$_firstname()$:"));
            var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
            tr1.append(jQuery("<td/>").append(fstname));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("$_lastname()$:"));
            var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
            tr2.append(jQuery("<td/>").append(sndname));
            table.append(tr2);

            var tr3 = jQuery("<tr/>").append(jQuery("<td/>").text("$_phone()$:"));
            var phone = jQuery("<input type='text' name='phone' autocomplete='off' />");
            tr3.append(jQuery("<td/>").append(phone));
            table.append(tr3);

            var tr4 = jQuery("<tr/>").append(jQuery("<td/>").text("$_companyposition()$:"));
            var companyposition = jQuery("<input type='text' name='companyposition' autocomplete='off' />");
            tr4.append(jQuery("<td/>").append(companyposition));
            table.append(tr4);

            var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("$_email()$"));
            var email = jQuery("<input type='text' name='email' autocomplete='off' />");
            tr5.append(jQuery("<td/>").append(email));
            table.append(tr5);

            body.append(table);


            Confirmation.popup({
              onAccept : function() {
                            new Submit({
                                method: "POST",
                                add : "true",
                                fstname : fstname.val(),
                                sndname : sndname.val(),
                                phone : phone.val(),
                                companyposition : companyposition.val(),
                                email : email.val(),
                                }).send();
                         },
              title : "$_modalAddCompanyAccountTitle()$",
              acceptButtonText : "$_modalAddCompanyAccountInviteButtonText()$",
              content  : body
            });
            }
      });


  var getFullname = function(user) {
    var fullname = user.field("fullname");
    if (user.field("fullname") == undefined || user.field("fullname").length < 2) {
      fullname = user.field("email");
    }
    return fullname;
  };

  var companyAccountsTable = KontraList.init({
    name : "CompanyAccountsTable",
    schema: new Schema({
      url: "/companyaccounts",
      sorting: new Sorting({ fields: ["fullname",
                                      "email",
                                      "role",
                                      "activated",
                                      "deletable"] }),
      paging: new Paging({}),
      filtering: new Filtering({ text: "", infotext: "$_SearchCompanyAccount()$" }),
      cells : [
        new Cell({name: "$_name()$", width: "100px", field:"fullname"}),
        new Cell({name: "$_email()$", width: "100px", field:"email" }),
        new Cell({name: "$_role()$", width: "100px", field:"role", special: "rendered",
                  rendering: function(value, idx, user) {
                    var label = "$_standard()$";
                    if (user.field("role")=="RoleAdmin") {
                      label = "$_admin()$";
                    } else if (user.field("role")=="RoleStandard") {
                      label = "$_standard()$";
                    } else if (user.field("role")=="RolePending") {
                      label = "$_pendingcompanyaccount()$";
                    }

                    if (user.field("isctxuser") || user.field("role")=="RolePending") {
                      var role = jQuery("<span>");
                      role.text(label);
                      return role;
                    } else {
                      var role = jQuery("<a>");
                      role.text(label);

                      var submitRoleChange = function() {
                        (new Submit({
                          url: "$currentlink$",
                          method: "POST",
                          changerole: true,
                          makeadmin: user.field("role")=="RoleStandard",
                          changeid: user.field("id")
                        })).send();
                      }
                      role.click(submitRoleChange);
                      return role;
                    }
                  }}),
        new Cell({width: "16px", field:"activated", special: "rendered",
                  rendering: function(value, idx, user) {
                    if (!user.field("activated")) {
                      var icon = jQuery("<a>");
                      icon.addClass("reminderForSendIcon");

                      var popupResendConfirmation = function() {
                        var submit = new Submit({
                          url: "$currentlink$",
                          method: "POST",
                          resend: "true",
                          resendid: user.field("id"),
                          resendemail: user.field("email")
                        });
                        var text = "$_modalResendCompanyAccountInviteBody()$ " + getFullname(user) + "?";
                        var content = jQuery("<p/>").text(text);
                        console.log("text: " + text);
                        Confirmation.popup({
                          submit: submit,
                          acceptText: "$_modalResendCompanyAccountInviteConfirm()$",
                          rejectText: "$_cancel()$",
                          title: "$_modalResendCompanyAccountInviteHeader()$",
                          content: content
                        });
                      };
                      icon.click(popupResendConfirmation);

                      return icon;
                    }
                    return jQuery("<span>");
                  }}),
        new Cell({width: "16px", field:"deletable", special: "rendered",
                  rendering: function(value, idx, user) {
                    if (!user.field("isctxuser")) {
                      if (user.field("deletable")) {
                        var icon = jQuery("<a>");
                        icon.addClass("icon");
                        icon.addClass("delete");

                        var popupDeleteConfirmation = function() {
                          var submit = new Submit({
                            url: "$currentlink$",
                            method: "POST",
                            remove: "true",
                            removeid: user.field("id")
                          });
                          var text = "$_listLocalDialogsBody()$" + getFullname(user) + "?";
                          var content = jQuery("<p/>").text(text);
                          console.log("text: " + text);
                          Confirmation.popup({
                            submit: submit,
                            acceptText: "$_ok()$",
                            rejectText: "$_cancel()$",
                            title: "$_delete()$",
                            content: content
                          });
                        };
                        icon.click(popupDeleteConfirmation);

                        return icon;
                      } else {
                        var icon = jQuery("<span>");
                        icon.addClass("icon");
                        icon.addClass("delete");
                        icon.addClass("gray");
                        return icon;
                      }
                    } else {
                      return jQuery("<span>");
                    }
                 }})
      ]
    }),
    headerExtras: addCompanyButton.input()
  });
    jQuery("#tab-viewer").append(companyAccountsTable.view.el);
  });
</script>
####
modalAddCompanyAccount=
  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-add-company-user" rel=".listForm">
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>$_modalAddCompanyAccountTitle()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <input type="hidden" name="add" value="true" />
          <p>
            $_modalAddCompanyAccountBody()$
          </p>
          <div class="account-body">
            <table>
              <tr>
                  <td>$_firstname()$:</td>
                  <td><input type="text" name="fstname" autocomplete="off" /></td>
              </tr>
              <tr>
                  <td>$_lastname()$:</td>
                  <td><input type="text" name="sndname" autocomplete="off" /></td>
              </tr>
              <tr>
                  <td>$_email()$:</td>
                  <td><input type="email" name="email" autocomplete="off"/></td>
              </tr>
            </table>
          </div>
        </div>
        <input type="submit" class="hidden" />
      </div>
      <div class="modal-footer">
        <a class="cancel close float-left" style="height:40px;">
          <div class="label">$_cancel()$</div>
        </a>
        <a class="btn-small green float-right submit" href="#">
          <div class="left"></div>
          <div class="label"><p>$_modalAddCompanyAccountInviteButtonText()$</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>
#####