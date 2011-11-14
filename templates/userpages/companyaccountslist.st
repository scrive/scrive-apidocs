####    Pages for users - usual from my account
viewCompanyAccounts =
<div>
  <div id="tab-viewer">
    <form method="post" action="$currentlink$" class="listForm">
      <div class="tab-header"></div>
      <ul class="tabs">
        <li><a href="/account">$_HeadUserInfo()$</a></li>
        <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        <li class="active"><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>
        <li><a href="/account/sharing">$_sharing()$</a></li>
        <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      </ul>
    </form>
    $modalAddCompanyAccount()$
  </div>
  $listLocalDialogs()$
</div>

<script>

  var createAddCompanyButton = function() {
    var addCompanyButton = Button.init({
        color: "green",
        size: "tiny",
        text: "$_newCompanyAccountButtonText()$"
      }).input();

    //maybe make a component for this sort of button?
    addCompanyButton.overlay({
      target: jQuery("#dialog-add-company-user"),
      mask: standardDialogMask,
      top: standardDialogTop,
      fixed: false
    });

    return addCompanyButton;
  };

  var companyAccountsTable = KontraList.init({
    name : "CompanyAccountsTable",
    schema: new Schema({
      url: "/companyaccounts",
      sorting: new Sorting({ fields: ["fullname",
                                      "email",
                                      "iscompanyadmin"] }),
      paging: new Paging({}),
      filtering: new Filtering({ text: "", infotext: "$_SearchCompanyAccount()$" }),
      cells : [
        new Cell({name: "ID", width:"20px", field:"id", special: "select"}),
        new Cell({name: "$_name()$", width:"65px", field:"fullname"}),
        new Cell({name: "$_email()$", width:"100px", field:"email" }),
        new Cell({name: "$_role()$", width: "100px", field:"iscompanyadmin", special: "rendered",
                  rendering: function(iscompanyadmin,idx,listobject) {
                    if (iscompanyadmin) {
                      return jQuery("<div>$_admin()$</div>");
                    } else {
                      return jQuery("<div>$_standard()$</div>");
                    }
                  }})
      ],
      options:
        [{ name: "$_delete()$",
           onSelect: function(users) {
              var submit = new Submit({
                                url: "$currentlink$",
                                method: "POST",
                                remove: "true",
                                archive: "true",
                                acccheck: _.map(users, function(user) { return user.field("id"); })
                          });
              var text = "$_listLocalDialogsBody()$"
              if (users.length == 1) {
                var fullname = users[0].field("firstname") + " " + users[0].field("lastname");
                if (fullname.length<2)
                  fullname = users[0].field("email");
                text += fullname + ".";
              } else {
                text += users.length + (" $_CompanyAccounts()$.").toLowerCase();
              }
              var content = jQuery("<p/>").text(text);
              Confirmation.popup({
                submit: submit,
                acceptText: "$_ok()$",
                rejectText: "$_cancel()$",
                title: "$_delete()$",
                content: content
              })
           }
         }]
    }),
    headerExtras: createAddCompanyButton
  });

  jQuery(function(){
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