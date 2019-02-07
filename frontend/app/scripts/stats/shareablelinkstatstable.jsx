var React = require("react");
var List = require("../lists/list");

module.exports = React.createClass({
  mixins: [List.ReloadableContainer],
  render: function () {
    var self = this;

    return (
      <List.List
        ref="list"
        url={self.props.url}
        loadLater={true}
        dataFetcher={function (d) { return d.stats; }}
      >
        <List.Column
          name={localization.account.stats.columnDate}
          width="60px"
          rendering={function (d) {
            return (<div>{d.field("date")}</div>);
          }}
        />

        <List.Column
          name={localization.account.stats.columnTemplateTitle}
          width="100px"
          className="sender"
          rendering={function (d) {
            return (
              <div className="expandable-stats"
                onClick={function () {
                  d.toggleExpand();
                }}
              >
                {d.field("name")}
              </div>
            );
          }}
        />

        <List.Sublist
          count={function (d) {
            return (d.field("template_stats") != undefined ? d.field("template_stats").length : 0);
          }}
          rendering={function (d, i) {
            return [
              <td key="1">
                <div className="expanded-stats-column">
                  {d.field("template_stats")[i].date}
                </div>
              </td>,
              <td key="2">
                <div className="expanded-stats-column">
                  {d.field("template_stats")[i].title}
                </div>
              </td>,
              <td key="3">
                <div className="expanded-stats-column">
                  {d.field("template_stats")[i].closed}
                </div>
              </td>,
              <td key="4">
                <div className="expanded-stats-column">
                  {d.field("template_stats")[i].sent}
                </div>
              </td>
            ];
           }}
        />

        <List.Column
          name={localization.account.stats.columnClosedDocuments}
          width="70px"
          rendering={function (d) {
            return (<div>{d.field("closed")}</div>);
          }}
        />

        <List.Column
          name={localization.account.stats.columnSendDocuments}
          width="70px"
          rendering={function (d) {
            return (<div>{d.field("sent")}</div>);
          }}
        />

      </List.List>
    );
  }
});
