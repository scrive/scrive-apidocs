var React = require("react");

var DaysStatsTableView = require("../../account/usersandstats/daysstatstable");

var StatsView = React.createClass({
  propTypes: {
    mode: React.PropTypes.string.isRequired,
    userId: React.PropTypes.string.isRequired
  },
  componentDidMount: function () {
    this.refs.table.reload();
  },
  render: function () {
    var url = (
      "/adminonly/useradmin/usagestats/" + this.props.mode + "/" +
      this.props.userId + "?withCompany=false"
    );

    return (
      <DaysStatsTableView ref="table" withCompany={false} url={url} />
    );
  }
});

var UserStatsView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    userId: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <div className="tab-content account usagestats">
        <h2>{localization.account.stats.last30days}</h2>
        <div className="jsdaytable">
          <StatsView mode="days" userId={this.props.userId} />
        </div>

        <h2>{localization.account.stats.last6months}</h2>
        <div className="jsmonthtable">
          <StatsView mode="months" userId={this.props.userId} />
        </div>
      </div>
    );
  }
});

module.exports = UserStatsView;
