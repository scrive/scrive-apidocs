var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Select = require("../../common/select");
var Track = require("../../common/track");
var List = require("../../lists/list");
var jQuery = require("jquery");
var NotEmptyValidation = require("../../../js/validation.js").NotEmptyValidation;
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var Submit = require("../../../js/submits.js").Submit;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Language = require("../../../js/utils/language.js").Language;
var $ = require("jquery");
var Modal = require("../../common/modal");
var InfoTextInput = require("../../common/infotextinput");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Subscription = require("../subscription");

var CreateAccountModal = require("./createaccountmodal");
var EditAccountModal = require("./editaccountmodal");

var AccountOperationModalMixin = {
  userFullName: function () {
    var fullname = this.props.user.field("fullname");
    if (fullname== undefined || fullname.length < 2) {
      fullname = this.props.user.field("email");
    }

    return fullname;
  }
}

var RemoveAccountModalContent = React.createClass({
  mixins: [AccountOperationModalMixin],
  propTypes: {
    user: React.PropTypes.object.isRequired
  },
  render: function () {
    return (
      <p>
        { /* if */ (this.props.user) &&
          <HtmlTextWithSubstitution
            secureText={localization.account.companyAccounts.deleteModalBody}
            subs={{
              ".put-one-or-more-things-to-be-deleted-here": this.userFullName(this.props.user)
            }}
          />
        }
      </p>
    );
  }
});

var ResendInvitationModalContent = React.createClass({
  mixins: [AccountOperationModalMixin],
  propTypes: {
    user: React.PropTypes.object.isRequired
  },
  render: function () {
    return (
      <p>
        { /* if */ (this.props.user) &&
          <span>{localization.account.companyAccounts.resendModalBody} {this.userFullName(this.props.user)}.</span>
        }
      </p>
    );
  }
});

module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    propTypes : {
      loadLater: React.PropTypes.bool
    },
    getInitialState: function () {
      return {
        showCreateAccountModal: false,
        showEditAccountModal: false,
        accountToRemove: null,
        accountToReinvite: null,
        accountToEdit: null,
        availableUserGroups: []
      };
    },
    componentDidMount: function () {
      if (this.props.loadLater === false) {
        this.reload();
      }
    },
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
    roleOptions: function () {
      return [{name: localization.account.companyAccounts.roleStandard, value: "RoleStandard"},
              {name: localization.account.companyAccounts.roleAdmin, value: "RoleAdmin"}];
    },
    showCreateAccountModal: function () {
      var self = this;
      
      var getUserID = function (consumeUserID) {
        new Submit({
          url: "/api/frontend/getprofile",
          method: "GET",
          ajax: true,
          ajaxsuccess: function(resp) {
            if (resp.id) {
              consumeUserID(resp.id)
            }
          }
        }).sendAjax();
      };

      var getUserAdminUGIDs = function(userID, consumeUGIDs) {
        new Submit({
          url: "/api/frontend/getuserroles/"+userID,
          method: "GET",
          ajax: true,
          ajaxsuccess: function(resp) {
            if (resp) {
              consumeUGIDs(_.uniq(resp.filter(function (value) {
                // get user_admins
                return value.role_type == "user_admin" && value.target.type == "user_group";
              }).map(function (role) {
                return role.target.id;
              })));
            }
          }
        }).sendAjax();
      }

      var getUserGroup = function(ugid, consumeUG) {
        new Submit({
          url: "/api/frontend/usergroups/"+ugid,
          method: "GET",
          ajax: true,
          ajaxsuccess: function(resp) {
            if (resp) {
              consumeUG(resp);
            }
          }
        }).sendAjax();
      }

      var mapCallbacks = function(inputArray, outputArray, worker, consumeOutputArray) {
        if (inputArray.length < 1) {
          consumeOutputArray(outputArray);
        } else {
          worker(inputArray[0], function(newOutputItem) {
            var newInArr = inputArray.slice(0); // shallow clone
            newInArr.shift();
            var newOutArr = outputArray.slice(0); // shallow clone
            newOutArr.push(newOutputItem);
            mapCallbacks(newInArr, newOutArr, worker, consumeOutputArray);
          });
        }
      }

      getUserID( function (uid) {
        getUserAdminUGIDs(uid, function (ugids) {
          if (ugids.length > 1) {
            mapCallbacks(ugids, [], getUserGroup, function(ugs) {
              Track.track('Click new account');
              self.setState({
                  showCreateAccountModal: true
                , availableUserGroups : ugs.slice(0)
              });
            });
          } else {
            Track.track('Click new account');
              self.setState({
                  showCreateAccountModal: true
                , availableUserGroups : []
              });
          }
        });
      });
    },
    onCreateAccountModalClose: function (reload) {
      if (reload === true) {
        this.reload();
      }

      this.setState({showCreateAccountModal: false});
    },
    showRemoveAccountModal: function (user) {
      Track.track("Click delete user");
      this.setState({
        showRemoveAccountModal: true,
        accountToRemove: user
      });
    },
    onRemoveAccountModalClose: function (reload) {
      if (reload === true) {
        this.reload();
      }

      this.setState({showRemoveAccountModal: false});
    },
    onRemoveAccountModalAccept: function () {
      var self = this;
      var accountToRemove = this.state.accountToRemove;

      if (accountToRemove) {
        Track.track('Click delete user');
        var submit = new Submit({
          url: "/account/companyaccounts/remove",
          method: "POST",
          ajax: true,
          ajaxsuccess: function(resp) {
            if (resp.removed) {
              new FlashMessage({
                type: "success",
                content: localization.account.companyAccounts.companyAccountDeleted
              });  
            } else {
              new FlashMessage({
                type: "error",
                content: localization.account.companyAccounts.deleteFailedHasDocuments
              });
            }
            
            self.onRemoveAccountModalClose(true);
          },
          removeid: accountToRemove.field("id"),
          removeemail: accountToRemove.field("email"),
          mixpanel: {
            name: "Accept",
            props: {
              "Accept" : "delete user"
            }
        }
        }).sendAjax();
      }
    },

    showEditAccountModal: function (user) {
      Track.track("Click edit user");
      this.setState({
        showEditAccountModal: true,
        accountToEdit: user
      });
    },
    onEditAccountModalClose: function (reload) {
      this.setState({showEditAccountModal: false});
    },
    onEditAccountModalAccept: function () {
      var self = this;
      self.setState({showEditAccountModal: false});
      window.setTimeout(function() { self.reload(); }, 500);
    },


    showResendInvitationModal: function (user) {
      this.setState({
        showResendInvitationModal: true,
        accountToReinvite: user
      });
    },
    onResendInvitationModalClose: function (reload) {
      if (reload === true) {
        this.reload();
      }

      this.setState({showResendInvitationModal: false});
    },
    onResendInvitationModalAccept: function () {
      var self = this;
      var accountToReinvite = this.state.accountToReinvite;

      if (accountToReinvite) {
        Track.track("Click resend confirmation");
        var submit = new Submit({
          url: "/account/companyaccounts/resend",
          ajax: true,
          ajaxsuccess: function(resp) {
            if (resp.resent) {
              new FlashMessage({
                type: "success",
                content: localization.account.companyAccounts.companyInviteResent
              });  
            }

            self.onResendInvitationModalClose(true);
          },
          method: "POST",
          resendid: accountToReinvite.field("id"),
          mixpanel: {
            name: "Accept",
            props: {"Accept": "resend confirmation"}
          }
        }).sendAjax();
      }
    },
    addingUsersEnabled: function () {
      return !Subscription.currentSubscription().hasFreePlan();
    },
    render: function() {
      var self = this;
      return (
        <div>
          <List.List
            ref='list'
            url="/companyaccounts"
            dataFetcher={function(d) {return d.accounts;}}
            loadLater={true}
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
              disabled={!self.addingUsersEnabled()}
              onSelect={this.showCreateAccountModal}
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
              name={localization.account.companyAccounts.columnCompany}
              sorting="company_name"
              width="260px"
              rendering={function(d) {
                return (<div>{d.field("company_name")}</div>);
              }}
            />

            <List.Column
              name={localization.account.twoFactor.twoFactorShortSection}
              sorting="twofactor_active"
              width="30px"
              rendering={function(d) {
                var active = d.field("twofactor_active");
                if (active) {
                  return (<a className='twofactorIcon'/>);
                } else {
                  return (<span/>);
                }
              }}
            />

            <List.Column
              name={localization.account.companyAccounts.columnRole}
              sorting="role"
              width="230px"
              rendering={function(d) {
                var canChangeRole = !d.field("isctxuser") || !d.field("role") =="RoleInvite";
                if (canChangeRole) {
                  return (<Select
                            isOptionSelected={function (o) {
                              return o.value == d.field("role");
                            }}
                            onSelect={function () {
                              self.changeRole(d);
                            }}
                            options={self.roleOptions()}
                          />);
                  return (<a onClick={function() {self.changeRole(d);}}>{self.roleText(d)}</a>);
                } else {
                  return (<span className='unchangable-role'>{self.roleText(d)}</span>);
                }
              }}
            />

            <List.Column
              name=""
              width="16px"
              rendering={function(d) {
                var role = d.field("role");
                if (role != "RoleInvite") {
                  return (
                      <a
                        className='edit icon'
                        style={{marginTop : "3px"}}
                        onClick={() => self.showEditAccountModal(d)}
                      />
                    );
                } else {
                  return (<span/>);
                }
              }}
            />

            <List.Column
              name=""
              width="16px"
              rendering={function(d) {
                var canBeReinvited = !d.field("activated");
                if (canBeReinvited) {
                  return (
                      <a
                        className='remind icon'
                        style={{marginTop : "3px"}}
                        onClick={() => self.showResendInvitationModal(d)}
                      />
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
                      <a
                        className='icon delete'
                        style={{marginTop : "3px"}}
                        onClick={() => self.showRemoveAccountModal(d)}
                      />
                    );
                } else {
                  return (<span/>);
                }
              }}
            />
          </List.List>

          <CreateAccountModal
            active={self.state.showCreateAccountModal}
            availableUserGroups={self.state.availableUserGroups}
            onClose={self.onCreateAccountModalClose}
          />

          <Modal.Container active={(self.state.showRemoveAccountModal)}>
            <Modal.Header
              title={localization.account.companyAccounts.deleteModalTitle}
              showClose={true}
              onClose={self.onRemoveAccountModalClose}
            />
            <Modal.Content>
              <RemoveAccountModalContent user={self.state.accountToRemove} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onRemoveAccountModalClose} />
              <Modal.AcceptButton type="cancel" onClick={self.onRemoveAccountModalAccept} />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={(self.state.showResendInvitationModal)}>
            <Modal.Header
              title={localization.account.companyAccounts.resendModalTitle}
              showClose={true}
              onClose={self.onResendInvitationModalClose}
            />
            <Modal.Content>
              <ResendInvitationModalContent user={self.state.accountToReinvite} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onResendInvitationModalClose} />
              <Modal.AcceptButton
                text={localization.account.companyAccounts.resendModalAccept}
                onClick={self.onResendInvitationModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <EditAccountModal
            active={self.state.showEditAccountModal}
            account={self.state.accountToEdit}
            onClose={self.onEditAccountModalClose}
            onAccept={self.onEditAccountModalAccept}
          />


        </div>
      );
    }
});
