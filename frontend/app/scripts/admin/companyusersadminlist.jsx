var React = require("react");
var List = require("../lists/list");
var moment = require("moment");
var jQuery = require("jquery");
var $ = require("jquery");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var Submit = require("../../js/submits.js").Submit;
var Modal = require("../common/modal");

var CreateUserModal = require("./createusermodal");

module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    getInitialState: function () {
      return {
        showCreateUserModal: false,
        createUserModalCompanyId: false,
        showDeleteInvitationModal: false,
        invitationToDelete: null
      };
    },
    accountLink : function(d) {
      if (d.field("id") != "") {
        return "/adminonly-old/useradmin/" + d.field("id");
      }
    },
    openDeleteInvitationModal : function(d) {
      this.setState({
        showDeleteInvitationModal: true,
        invitationToDelete: d
      });
    },
    onDeleteInvitationModalClose: function () {
      this.setState({
        showDeleteInvitationModal: false,
        invitationToDelete: null
      });
    },
    onDeleteInvitationModalAccept: function () {
      var d = this.state.invitationToDelete;

      var self = this;
      new Submit({
        url: "/adminonly/useradmin/deleteinvite/" + self.props.companyid + "/" + d.field("id"),
        method: "POST",
        ajaxsuccess: function() {
          self.reload();
          self.onDeleteInvitationModalClose();
        },
        ajaxerror: function() {
          new FlashMessage({type: 'error', content : "Failed"});
          self.onDeleteInvitationModalClose();
        }
      }).sendAjax();
    },
    onCreateUserModalClose: function (reload) {
      if (reload === true) {
        this.reload();
      }

      this.setState({
        showCreateUserModal: false,
        createUserModalCompanyId: null
      });
    },
    render: function() {
      var self = this;
      return (
        <div>
          <List.List
            url={"/adminonly/companyaccounts/" + self.props.companyid}
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
            ref='list'
          >
           <List.TextFiltering text={"Company Accounts" } />

           <List.ListAction
              name="Add new user in company"
              type="action"
              size="tiny"
              onSelect={function() {
                self.setState({
                  showCreateUserModal: true,
                  createUserModalCompanyId: self.props.companyid
                });
              }}
            />

           <List.Column
             name="Name"
             width="100px"
             sorting="fullname"
             rendering={function(d) {
               return (<a href={self.accountLink(d)}>{d.field("fullname")}</a>);
             }}
           />
           <List.Column
             name="Email"
             width="100px"
             sorting="email"
             rendering={function(d) {
                return (<a href={self.accountLink(d)}>{d.field("email")}</a>);
             }}
           />

           <List.Column
             name="Role"
             width="100px"
             sorting="role"
             rendering={function(d) {
               if (d.field("role") =="RoleAdmin") {
                 return (<span>Admin</span>);
               } else if (d.field("role") =="RoleStandard") {
                 return (<span>Standard</span>);
               } else if (d.field("role") =="RoleInvite") {
                 return (<span>!User in different company!</span>);
               } else {
                 return (<span/>);
               }
             }}
           />

           <List.Column
             name="TOS date"
             width="80px"
             rendering={function(d) {
               if (d.field("role") =="RoleInvite") {
                 return (<a style={{color:"red"}} href="#" onClick={function() {self.openDeleteInvitationModal(d)}}> Click to delete this invitation </a>);
               }
               var time = d.field("tos");
               if (time != undefined && time != "") {
                 return (<small> {moment(time).toDate().toTimeAbrev()}</small>);
               } else
                 return (<span/>);
             }}
           />

           <List.Column
             name="2FA"
             width="20px"
             rendering={function(d) {
               return (<small> {d.field("twofactor_active") ? "On" : "" } </small>);
             }}
           />

         </List.List>

         <CreateUserModal
           active={this.state.showCreateUserModal}
           companyid={this.state.createUserModalCompanyId}
           onClose={this.onCreateUserModalClose}
         />

         <Modal.Container active={this.state.showDeleteInvitationModal}>
          <Modal.Header
            title="Delete invitation"
            showClose={true}
            onClose={this.onDeleteInvitationModalClose}
          />
          <Modal.Content>
            <div style={{textAlign: "center"}}>
              Are you sure that you want to delete this invitation?<br/>
              User will still exist in his original company.
            </div>
          </Modal.Content>
          <Modal.Footer>
            <Modal.CancelButton onClick={this.onDeleteInvitationModalClose} />
            <Modal.AcceptButton onClick={this.onDeleteInvitationModalAccept} />
          </Modal.Footer>
         </Modal.Container>

       </div>
     );
   }
});
