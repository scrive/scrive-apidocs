/** @jsx React.DOM */

define(['React', 'common/backbone_mixin', 'common/hubspot_service', 'lists/list','legacy_code'], function(React, BackboneMixin, HubSpot, List) {

var userFullName = function(d) {
    var fullname = d.field("fullname");
    if (fullname== undefined || fullname.length < 2) {
      fullname = d.field("email");
    }
    return fullname;
};

var openCreateAccountPopup = function(callback) {
            mixpanel.track('Click new account');
            var body = jQuery("<div class='standard-input-table'>");
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.fstname));
            var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
            tr1.append(jQuery("<td/>").append(fstname));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.sndname));
            var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
            tr2.append(jQuery("<td/>").append(sndname));
            table.append(tr2);

            var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.email));
            var email = jQuery("<input type='text' name='email' autocomplete='off' />");
            tr5.append(jQuery("<td/>").append(email));
            table.append(tr5);

            body.append(table);


            var popup = new Confirmation({
              onAccept : function() {

                 var errorCallback = function(t,e,v) {   e.css("border-color", "red"); };
                 fstname.css("border-color", "");
                 sndname.css("border-color", "");
                 email.css("border-color", "");

                  var vresult = [
                                 fstname.validate(new NameValidation({callback: errorCallback, message: "Wrong first name format!"})),
                                 sndname.validate(new NameValidation({callback: errorCallback, message: "Wrong second name format!"})),
                                 email.validate((new NotEmptyValidation({callback: errorCallback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: errorCallback})))
                                ];

                  if (_.every(vresult, function(a) {return a;})) {

                    new Submit({
                        url: "/account/companyaccounts/add",
                        method: "POST",
                        fstname : fstname.val(),
                        sndname : sndname.val(),
                        email : email.val(),
                        ajax : true,
                        ajaxsuccess : function(resp) {
                          callback();
                          if (JSON.parse(resp).added)
                             new FlashMessage({type : "success", content : localization.account.companyAccounts.companyInviteSent});
                          else {
                             if (JSON.parse(resp).samecompany)
                                new FlashMessage({type : "success", content : localization.account.companyAccounts.companyInviteNotSentSameCompany});
                             else
                                new FlashMessage({type : "error", content : localization.account.companyAccounts.companyInviteNotSent});
                          }
                          popup.close();
                          HubSpot.track(HubSpot.FORM_INVITE,
                                        { "email" : email.val(),
                                          "language" : Language.current(),
                                          "scrive_domain" : location.hostname,
                                          "signup_method" : "CompanyInvitation",
                                          "firstname" : fstname.val(),
                                          "lastname" : sndname.val() }, true);
                        },
                        mixpanel : {name : 'Accept',  props : {'Accept' : 'new account'}}
                    }).sendAjax();

                 }
              },
              title : localization.account.companyAccounts.createNewModalTitle,
              width: 533,
              acceptButtonText : localization.account.companyAccounts.createNewModalAcceptButton,
              content: $("<div>").append($("<div class='modal-subtitle'>").html(localization.account.companyAccounts.createNewModalBody)).append(body)
            });
};

var openRemoveUserPopup = function(d,callback) {
  mixpanel.track('Click delete user');
  var confirmationText = $('<span />').html(localization.account.companyAccounts.deleteModalBody);
  var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here').text(userFullName(d));
  var content = jQuery("<p/>").append(confirmationText);
  var popup = new Confirmation({
    acceptText: localization.ok,
    rejectText: localization.cancel,
    title: localization.account.companyAccounts.deleteModalTitle,
    content: content,
    onAccept: function() {
      mixpanel.track('Click delete user');
      var submit = new Submit({
        url: "/account/companyaccounts/remove",
        method: "POST",
        ajax : true,
        ajaxsuccess : function(resp) {
          popup.close();
          if (JSON.parse(resp).removed)
            new FlashMessage({type : "success", content : localization.account.companyAccounts.companyAccountDeleted});
          else
            new FlashMessage({type : "error", content : localization.account.companyAccounts.deleteFailedHasDocuments});
          callback();
        },
      removeid: d.field("id"),
      removeemail: d.field("email"),
      mixpanel : {name : 'Accept', props : {'Accept' : 'delete user'}}
    }).sendAjax();
    }
  });
};

var openResendInvitationPopup = function(d,callback) {
  var popup = new Confirmation({
                                  onAccept: function() {
                                     mixpanel.track('Click resend confirmation');
                                      var submit = new Submit({
                                        url: "/account/companyaccounts/resend",
                                        ajax : true,
                                        ajaxsuccess : function(resp) {
                                          popup.close();
                                          if (JSON.parse(resp).resent)
                                              new FlashMessage({type : "success", content : localization.account.companyAccounts.companyInviteResent});
                                          callback();
                                        },
                                        method: "POST",
                                        resendid: d.field("id"),
                                        mixpanel : {name : 'Accept',
                                                      props : {'Accept' : 'resend confirmation'}}
                                      }).sendAjax();
                                  },
                                  acceptText: localization.account.companyAccounts.resendModalAccept,
                                  rejectText: localization.cancel,
                                  title: localization.account.companyAccounts.resendModalTitle,
                                  content: $("<p/>").text(localization.account.companyAccounts.resendModalBody + userFullName(d) + ".")
                                });
};


return React.createClass({
    mixins : [List.ReloadableContainer],
    changeRole : function(d) {
      var self = this;
      new Submit({
        url: "/account/companyaccounts/changerole",
        method: "POST",
        makeadmin: d.field("role") == "RoleStandard",
        changeid: d.field("id")
      }).sendAjax(function() {
        self.reload();
      });
    },
    roleText : function(d) {
       var label = localization.account.companyAccounts.roleStandard;
       if (d.field("role") == "RoleInvite" || d.field("tos") == undefined) {
         label =  localization.account.companyAccounts.rolePending;
       } else if (d.field("role") =="RoleAdmin") {
         label = localization.account.companyAccounts.roleAdmin;
       } else if (d.field("role") =="RoleStandard") {
         label =  localization.account.companyAccounts.roleStandard;
       }
       return label;
    },
    render: function() {
      var self = this;
      return (
          <List.List
            ref='list'
            url="/companyaccounts"
            dataFetcher={function(d) {return d.accounts;}}
            loadLater={self.props.loadLater}
            paramsFunction = {function(text,_selectfiltering,sorting,offset) {
              var params  = {};
              if (text) {
                params.text = text;
              }
              if (sorting.current()) {
                params.sorting = sorting.current();
                params.order = sorting.isAsc() ? "ascending" : "descending";
              }
              return params;
            }}
          >
            <List.TextFiltering text={localization.account.companyAccounts.search}/>

            <List.ListAction
              name={localization.account.companyAccounts.createNewButtonText}
              type="action"
              onSelect={function() {
                openCreateAccountPopup(function() {self.reload();});
              }}
            />
            <List.Column
              name={localization.account.companyAccounts.columnName}
              sorting="fullname"
              width="260px"
              rendering={function(d) {
                return (<div>{d.field("fullname")}</div>);
              }}
            />

            <List.Column
              name={localization.account.companyAccounts.columnEmail}
              sorting="email"
              width="260px"
              rendering={function(d) {
                return (<div>{d.field("email")}</div>);
              }}
            />

            <List.Column
              name={localization.account.companyAccounts.columnRole}
              sorting="role"
              width="260px"
              rendering={function(d) {
                var canChangeRole = !d.field("isctxuser") || !d.field("role") =="RoleInvite";
                if (canChangeRole) {
                  return (<a onClick={function() {self.changeRole(d);}}>{self.roleText(d)}</a>);
                } else {
                  return (<span>{self.roleText(d)}</span>);
                }
              }}
            />

            <List.Column
              name=""
              width="16px"
              sorting="activated"
              rendering={function(d) {
                var canBeReinvited = !d.field("activated");
                if (canBeReinvited) {
                  return (
                      <a className='remind icon' style={{marginTop : "3px"}} onClick={function() {openResendInvitationPopup(d,function() {self.reload();})} }/>
                    );
                } else {
                  return (<span/>);
                }
              }}
            />

            <List.Column
              name=""
              width="32px"
              sorting="deletable"
              rendering={function(d) {
                var canBeDeleted = (!d.field("isctxuser")) && d.field("deletable");
                if (canBeDeleted) {
                  return (
                      <a className='icon delete' style={{marginTop : "3px"}} onClick={function() {openRemoveUserPopup(d,function() {self.reload();})} }/>
                    );
                } else {
                  return (<span/>);
                }
              }}
            />
          </List.List>
      );
    }
});


});

