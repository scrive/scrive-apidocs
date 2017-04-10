var React = require("react");
var List = require("../../lists/list");


module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    render: function() {
      var self = this;
      return (
          <List.List
            ref='list'
            url={self.props.url}
            loadLater={true}
            dataFetcher={function(d) {return d.stats;}}
          >

            <List.Column
              name={localization.account.stats.columnDate}
              width="60px"
              rendering={function(d) {
                return (<div>{d.field("date")}</div>);
              }}
            />
            {/*if*/ (self.props.withCompany) &&
              (
                <List.Column
                  name={localization.account.stats.columnSender}
                  width="100px"
                  className="sender"
                  rendering={function(d) {
                    return (<div onClick={function() {d.toggleExpand();}}>{d.field("name")}</div>);
                  }}
                />
             )
            }

            {/*if*/ (self.props.withCompany) &&
              (
                <List.Sublist
                  count={function(d) {return d.field("user_stats") != undefined ? d.field("user_stats").length : 0}}
                  rendering={function(d,i) {
                    return [
                      <td key="1"><div style={{"marginLeft":"10px"}}>{d.field("user_stats")[i].date}</div></td>,
                      <td key="2"><div style={{"marginLeft":"10px"}}>{d.field("user_stats")[i].name}</div></td>,
                      <td key="3"><div style={{"marginLeft":"10px"}}>{d.field("user_stats")[i].closed}</div></td>,
                      <td key="4"><div style={{"marginLeft":"10px"}}>{d.field("user_stats")[i].sent}</div></td>,
                      <td key="5"><div style={{"marginLeft":"10px"}}>{d.field("user_stats")[i].signatures}</div></td>
                    ];
                   }}
                />
              )
            }

            <List.Column
              name={localization.account.stats.columnClosedDocuments}
              width="70px"
              rendering={function(d) {
                return (<div>{d.field("closed")}</div>);
              }}
            />

            <List.Column
              name={localization.account.stats.columnSendDocuments}
              width="70px"
              rendering={function(d) {
                return (<div>{d.field("sent")}</div>);
              }}
            />

            <List.Column
              name={localization.account.stats.columnClosedSignatures}
              width="80"
              rendering={function(d) {
                return (<div>{d.field("signatures")}</div>);
              }}
            />

          </List.List>
      );
    }
});
