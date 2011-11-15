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

  (function(window) {
     window.ListDeleteButtonModel = Backbone.Model.extend({
        initialize: function(args) {
          _.bindAll(this, 'change');
          _.bindAll(this, 'listObjectId',
                          'listObjectLabel',
                          'isEnabled',
                          'isVisible');
          this.set({ listObjectId: args.listObjectId,
                     listObjectLabel: args.listObjectLabel,
                     enabled: args.enabled,
                     visible: args.visible });
        },
        listObjectId: function() {
          return this.get("listObjectId");
        },
        listObjectLabel: function() {
          return this.get("listObjectLabel");
        },
        isEnabled: function() {
          return this.get("enabled")==undefined || this.get("enabled");
        },
        isVisible: function() {
          return this.get("visible")==undefined || this.get("visible");
        }
     });

     window.ListDeleteButtonView = Backbone.View.extend({
       model: ListDeleteButtonModel,
       initialize: function() {
         _.bindAll(this, 'createElems', 'render', 'popupDeleteConfirmation');
         this.model.bind('change', this.render);
         this.model.view = this;
         this.createElems();
         this.render();
       },
       createElems : function() {

         var enabledIcon = jQuery("<a>");
         enabledIcon.addClass("icon");
         enabledIcon.addClass("delete");
         enabledIcon.click(this.popupDeleteConfirmation);
         enabledIcon.hide();
         this.enabledIcon = enabledIcon;
         this.el.append(this.enabledIcon);

         var disabledIcon = jQuery("<span>");
         disabledIcon.addClass("icon");
         disabledIcon.addClass("delete");
         disabledIcon.addClass("gray");
         disabledIcon.hide();
         this.disabledIcon = disabledIcon;
         this.el.append(this.disabledIcon);
       },
       render: function() {
         if (!this.model.isVisible()) {
           this.enabledIcon.hide();
           this.disabledIcon.hide();
         } else if (this.model.isVisible() && !this.model.isEnabled()) {
           this.enabledIcon.hide();
           this.disabledIcon.show();
         } else {
           this.disabledIcon.hide();
           this.enabledIcon.show();
         }
       },
       popupDeleteConfirmation: function() {
         var submit = new Submit({
           url: "$currentlink$",
           method: "POST",
           remove: "true",
           removeid: this.model.listObjectId
         });
         var text = "$_listLocalDialogsBody()$" + this.model.listObjectLabel() + "?";
         var content = jQuery("<p/>").text(text);
         console.log("text: " + text);
         Confirmation.popup({
           submit: submit,
           acceptText: "$_ok()$",
           rejectText: "$_cancel()$",
           title: "$_delete()$",
           content: content
         });
       }
     });

     window.ListDeleteButton = {
       init: function(args) {
         this.model = new ListDeleteButtonModel({
           listObjectId: args.listObjectId,
           listObjectLabel: args.listObjectLabel,
           visible: args.visible,
           enabled: args.enabled
         });
         this.view = new ListDeleteButtonView({
           model: this.model,
           el: jQuery("<span>")
         });
         return this;
       }
     };

     window.RoleModel = Backbone.Model.extend({
        initialize: function(args) {
          _.bindAll(this, 'change');
          _.bindAll(this, 'listObjectId',
                          'isAdmin',
                          'isEnabled');
          this.set({ listObjectId: args.listObjectId,
                     admin: args.admin,
                     enabled: args.enabled });
        },
        listObjectId: function() {
          return this.get("listObjectId");
        },
        isAdmin: function() {
          return this.get("admin")==undefined || this.get("admin");
        },
        isEnabled: function() {
          return this.get("enabled")==undefined || this.get("enabled");
        }
     });

     window.RoleView = Backbone.View.extend({
       model: RoleModel,
       initialize: function() {
         _.bindAll(this, 'createElems', 'render', 'submitRoleChange');
         this.model.bind('change', this.render);
         this.model.view = this;
         this.createElems();
         this.render();
       },
       createElems : function() {
         var enabledRole = jQuery("<a>");
         enabledRole.hide();
         enabledRole.click(this.submitRoleChange);
         this.enabledRole = enabledRole;
         this.el.append(this.enabledRole);

         var disabledRole = jQuery("<span>");
         disabledRole.hide();
         this.disabledRole = disabledRole;
         this.el.append(this.disabledRole);
       },
       render: function() {
         var label = "$_standard()$";
         if (this.model.isAdmin()) {
           label = "$_admin()$";
         }

         if (!this.model.isEnabled()) {
           this.enabledRole.hide();
           this.disabledRole.text(label);
           this.disabledRole.show();
         } else {
           this.disabledRole.hide();
           this.enabledRole.text(label);
           this.enabledRole.show();
         }
       },
       submitRoleChange: function() {
         (new Submit({
           url: "$currentlink$",
           method: "POST",
           changerole: true,
           makeadmin: !this.model.isAdmin(),
           changeid: this.model.listObjectId
         })).send();
       }
     });

     window.Role = {
       init: function(args) {
         this.model = new RoleModel({
           listObjectId: args.listObjectId,
           admin: args.admin,
           enabled: args.enabled
         });
         this.view = new RoleView({
           model: this.model,
           el: jQuery("<span>")
         });
         return this;
       }
     };
  })(window);

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
        new Cell({name: "$_name()$", width:"150px", field:"fullname"}),
        new Cell({name: "$_email()$", width:"150px", field:"email" }),
        new Cell({name: "$_role()$", width: "100px", field:"iscompanyadmin", special: "rendered",
                  rendering: function(value, idx, user) {
                    return (Role.init({
                      listObjectId: user.field("id"),
                      admin: user.field("iscompanyadmin"),
                      enabled: !user.field("isctxuser")
                    })).view.el;
                  }}),
        new Cell({name: "", width: "100px", field:"isctxuser", special: "rendered",
                  rendering: function(value, idx, user) {

                    var fullname = user.field("fullname");
                    if (user.field("fullname") == undefined || user.field("fullname").length < 2) {
                      fullname = user.field("email");
                    }

                    var deletebutton = (ListDeleteButton.init({
                      listObjectId: user.field("id"),
                      listObjectLabel: fullname,
                      visible: !user.field("isctxuser"),
                      enabled: user.field("isdeletable")
                    })).view.el;

                    if (!user.field("isdeletable")) {
                      ToolTip.set({ on: deletebutton,
                                    tip: "$_toolTipUserHasLiveDocs()$" });
                    }

                    return deletebutton;
                  }})
      ]
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