var React = require("react");
var List = require("../lists/list");
var Submit = require("../../js/submits.js").Submit;
var _ = require("underscore");


module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    createNewDomain: function() {
      var self = this;
      new Submit({
        method: "POST",
        url: "/adminonly/brandeddomain/create",
        ajax: true,
        ajaxsuccess: function(js) {
          self.props.onSelect(js.id);
        }
      }).send();
    },
    render: function() {
      var self = this;
      return (
        <List.List
          url='/adminonly/brandeddomainslist'
          dataFetcher={function(d) {
            // We don't show main domain on this list
            var domainsWithoutMain = _.filter(d.domains,function(d) {return d.mainDomain != true});
            return domainsWithoutMain;
          }}
          idFetcher={function(d) {return d.field("id");}}
          loadLater={self.props.loadLater}
          paramsFunction = {function(text,_selectfiltering,sorting,offset) {
            var params  = {};
            if (text) {
              params.text = text;
            }
            return params;
            }}
          ref='list'
        >
          <List.TextFiltering text={"URL"} />
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
            name="Email originator"
            rendering={function(d) {
              return (<a onClick={function() {self.props.onSelect(d.field("id"));}}>{d.field("emailOriginator")}</a>);
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
