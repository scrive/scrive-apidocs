/** @jsx React.DOM */

define(['React', 'lists/list','legacy_code'], function(React, List) {

return React.createClass({
    mixins : [List.ReloadableContainer],
    createNewDomain: function() {
      var self = this;
      new Submit({
        method: "POST",
        url: "/adminonly/brandeddomain/create",
        ajax: true,
        ajaxsuccess: function(js) {
          self.props.onSelect(js.id);
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
          idFetcher={function(d) {return d.field("id");}}
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
            width="30px"
            rendering={function(d) {
              return (<a onClick={function() {self.props.onSelect(d.field("id"));}}>{d.field("id")}</a>);
            }}
          />
          <List.Column
            name="URL"
            rendering={function(d) {
              return (<a onClick={function() {self.props.onSelect(d.field("id"));}}>{d.field("url")}</a>);
            }}
          />
          <List.Column
            name="Contact email"
            rendering={function(d) {
              return (<a onClick={function() {self.props.onSelect(d.field("id"));}}>{d.field("contact_email")}</a>);
            }}
          />
          <List.Column
            name="Email originator"
            rendering={function(d) {
              return (<a onClick={function() {self.props.onSelect(d.field("id"));}}>{d.field("email_originator")}</a>);
            }}
          />
          <List.Column
            width="40px"
            rendering={function(d) {
              var logo = d.field("favicon");
              if (logo)
                return (<img style={{maxHeight: "25px", maxWidth: "50px"}} src={logo}/>);
              else
                return (<div/>);
            }}
          />
        </List.List>
      );
    }
});

});
