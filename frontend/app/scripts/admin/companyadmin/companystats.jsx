var React = require("react");

var StatsTableView = require("../../account/usersandstats/statstable");

var StatsView = React.createClass({
  propTypes: {
    mode: React.PropTypes.string.isRequired,
    companyId: React.PropTypes.string.isRequired
  },
  componentDidMount: function () {
    this.refs.table.reload();
  },
  render: function () {
    var url = (
      "/adminonly/companyadmin/usagestats/" + this.props.mode + "/" +
      this.props.companyId + "?withCompany=true"
    );

    return (
      <StatsTableView ref="table" withCompany={true} url={url} />
    );
  }
});

var CompanyStatsView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    companyId: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <div className="tab-content account usagestats">
        <h2>{localization.account.stats.last30days}</h2>
        <div className="jsdaytable">
          <StatsView mode="days" companyId={this.props.companyId} />
        </div>

        <h2>{localization.account.stats.last6months}</h2>
        <div className="jsmonthtable">
          <StatsView mode="months" companyId={this.props.companyId} />
        </div>
      </div>
    );
  }
});

module.exports = CompanyStatsView;
