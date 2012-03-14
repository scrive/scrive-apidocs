####    Pages for users - usual from my account
viewCompanyAccounts =
<div class="tab-viewer">
      <div class="tab-header"></div>
      <ul class="tabs">
        <li><a href="/account">$_HeadUserInfo()$</a></li>
        <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        <li><a href="/account/company">$_CompanySettings()$</a></li>
        <li class="active"><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>
        <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
        <li><a href="/account/usagestats">$_HeadUsageStats()$</a></li>z
      </ul>
</div>


<script>
 jQuery(function(){
    var addCompanyAccountButton = Button.init({
        color: "green",
        size: "tiny",
        text: "$_newCompanyAccountButtonText()$",
        onClick: function() {
            var body = jQuery("<div class='account-body'>");
            body.append("<p>$_modalAddCompanyAccountBody()$</p>");
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("$_firstname()$:"));
            var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
            tr1.append(jQuery("<td/>").append(fstname));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("$_lastname()$:"));
            var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
            tr2.append(jQuery("<td/>").append(sndname));
            table.append(tr2);

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
                                email : email.val()
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

  var companyAccountsTable = KontraList().init({
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
                            removeid: user.field("id"),
                            removeemail: user.field("email")
                          });
                          var text = "$_listLocalDialogsBody()$ " + getFullname(user) + "?";
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

                        ToolTip.set({ on: icon,
                                      tip: "$flashMessageUserHasLiveDocs()$" });
                        return icon;
                      }
                    } else {
                      return jQuery("<span>");
                    }
                 }})
      ]
    }),
    headerExtras: addCompanyAccountButton.input()
  });
    jQuery(".tab-viewer").append(companyAccountsTable.view.el);
  });
</script>
####
