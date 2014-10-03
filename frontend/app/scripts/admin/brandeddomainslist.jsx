/** @jsx React.DOM */

define(['React', 'lists/list','legacy_code'], function(React, List) {

return React.createClass({
    reload : function() {
      if (this.refs.list)
        this.refs.list.reload();
    },
    createNewDomain: function() {
      new Submit({
        method: "POST",
        url: "/adminonly/brandeddomain/create",
        ajax: true,
        ajaxsuccess: function(js) {
          window.location = "/adminonly/brandeddomain/" + js.id;
        },
        expectedType: "json"
      }).send();
    },
    render: function() {
      var self = this;
      return (
        <List.List
          url='/adminonly/brandeddomainslist'
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >
          <List.ListAction
            name="Create branded domain"
            onSelect={function() {
              self.createNewDomain();
            }}
          />
          <List.Column
            name="URL"
            width="20px"
            rendering={function(d) {
              return (<a href={"/adminonly/brandeddomain/"+d.field("fields").id}>{d.field("fields").id}</a>);
            }}
          />
          <List.Column
            name="URL"
            width="80px"
            rendering={function(d) {
              return (<a href={"/adminonly/brandeddomain/"+d.field("fields").id}>{d.field("fields").url}</a>);
            }}
          />
          <List.Column
            name="Contact email"
            width="80px"
            rendering={function(d) {
              return (<a href={"/adminonly/brandeddomain/"+d.field("fields").id}>{d.field("fields").contact_email}</a>);
            }}
          />
          <List.Column
            name="Email originator"
            width="80px"
            rendering={function(d) {
              return (<a href={"/adminonly/brandeddomain/"+d.field("fields").id}>{d.field("fields").email_originator}</a>);
            }}
          />
          <List.Column
            name="SMS originator"
            width="80px"
            rendering={function(d) {
              return (<a href={"/adminonly/brandeddomain/"+d.field("fields").id}>{d.field("fields").sms_originator}</a>);
            }}
          />
          <List.Column
            name="Logo"
            width="80px"
            rendering={function(d) {
              var logo = d.field("fields").logo;
              if (logo)
                return (<img style={{maxHeight: "25px", maxWidth: "50px"}} src={logo}/>);
              else
                return (<div/>);
            }}
          />
          <List.Pagination/>
        </List.List>
      );
    }
});

});
