var React = require("react");

var ShareableLinkStatsTableView = require("./shareablelinkstatstable");

var SCOPE_DAYS = "days";
var SCOPE_MONTHS = "months";

var ShareableLinkStatsView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    userId: React.PropTypes.string,
    groupId: React.PropTypes.string
  },

  componentDidMount: function () {
    this.refs.tableDays.reload();
    this.refs.tableMonths.reload();
  },

  baseUrl: function () {
    if (this.props.groupId) {
      return "/adminonly/companyadmin/shareablelinkstats";
    } else if (this.props.userId) {
      return "/adminonly/useradmin/shareablelinkstats";
    } else {
      return "/api/v2/usagestats/shareablelink";
    }
  },

  enrichUrl: function (url) {
    var result = url;
    if (this.props.groupId) {
      result += "/" + this.props.groupId;
    } else if (this.props.userId) {
      result += "/" + this.props.userId;
    }
    return result;
  },

  urlForScope: function (scope) {
    return this.enrichUrl(this.baseUrl() + "/" + scope);
  },

  render: function () {
    return (
      <div className="tab-content account usagestats">
        <h2>{localization.account.stats.last30days}</h2>
        <div className="jsdaytable">
          <ShareableLinkStatsTableView
            ref="tableDays"
            url={this.urlForScope(SCOPE_DAYS)}
          />
        </div>
        <div className="usagestatsjson">
          <a href={this.urlForScope(SCOPE_DAYS)} target="_blank">
            {localization.account.stats.openAsJson}
          </a>
        </div>

        <h2>{localization.account.stats.last6months}</h2>
        <div className="jsmonthtable">
          <ShareableLinkStatsTableView
            ref="tableMonths"
            url={this.urlForScope(SCOPE_MONTHS)}
          />
        </div>
        <div className="usagestatsjson">
          <a href={this.urlForScope(SCOPE_MONTHS)} target="_blank">
            {localization.account.stats.openAsJson}
          </a>
        </div>
      </div>
    );
  }
});

module.exports = ShareableLinkStatsView;
