var React = require("react");
var List = require("../lists/list");
var moment = require("moment");
var jQuery = require("jquery");
var $ = require("jquery");
var _ = require("underscore");

var CreateUserModal = require("./createusermodal");

var usersLimit = 100;

module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    getInitialState: function () {
      return {
        showCreateUserModal: false
      };
    },
    userLink : function(d) {
      return "/adminonly-old/useradmin/" + d.field("id");
    },
    onCreateUserModalClose: function (reload) {
      if (reload === true) {
        this.reload();
      }

      this.setState({showCreateUserModal: false});
    },
    render: function() {
      var self = this;
      return (
        <div>
          <List.List
            url={"/adminonly/userslist"}
            dataFetcher={function(d) {return _.first(d.users,usersLimit);}}
            totalCountFunction={function(data,list_offset){ return data.users.length + list_offset;}}
            idFetcher={function(d) {return d.field("id");}}
            loadLater={self.props.loadLater}
            paramsFunction = {function(text,_selectfiltering,sorting,offset) {
              var params  = {
                // Limit is set to more than amount of companies we are displaing. This way we can get information if there are more pages.
                limit: usersLimit + 1,
                offset : offset
              };
              if (text) {
                params.text = text;
              }
              if (sorting.current() == "tos") {
                params.tosSorting = sorting.isAsc() ? "ascending" : "descending";
              }
              return params;
            }}
            ref='list'
          >
           <List.TextFiltering text={"Username, email or company name"} />

           <List.ListAction
              name="Create user with empty company"
              type="action"
              size="tiny"
              onSelect={function() {
               self.setState({showCreateUserModal: true});
              }}
            />

           <List.Column
             name="Username"
             width="130px"
             rendering={function(d) {
               return (<a href={self.userLink(d)}>{d.field("username")}</a>);
             }}
           />

           <List.Column
             name="Email"
             width="130px"
             rendering={function(d) {
               return (<a href={self.userLink(d)}>{d.field("email")}</a>);
             }}
           />

           <List.Column
             name="Company"
             width="100px"
             rendering={function(d) {
               return (<a href={self.userLink(d)}>{d.field("company")}</a>);
             }}
           />

           <List.Column
             name="Position"
             width="100px"
             rendering={function(d) {
               return (<a href={self.userLink(d)}>{d.field("companyposition")}</a>);
             }}
           />

           <List.Column
             name="Phone"
             width="100px"
             rendering={function(d) {
               return (<a href={self.userLink(d)}>{d.field("phone")}</a>);
             }}
           />

           <List.Column
             name="TOS date"
             width="80px"
             sorting="tos"
             rendering={function(d) {
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


           <List.Pagination maxNextPages={1}/>
         </List.List>

         <CreateUserModal
           active={this.state.showCreateUserModal}
           onClose={this.onCreateUserModalClose}
         />
       </div>
     );
   }
});
