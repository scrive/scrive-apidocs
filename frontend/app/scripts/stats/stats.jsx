var React = require("react");

var StatsTableView = require("./statstable");

var SCOPE_DAYS = "days";
var SCOPE_MONTHS = "months";

var StatsView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    userId: React.PropTypes.string,
    companyId: React.PropTypes.string,
    withCompany: React.PropTypes.bool.isRequired
  },
  componentDidMount: function () {
    this.refs.tableDays.reload();
    this.refs.tableMonths.reload();
  },
  baseUrl: function () {
    if (this.props.userId) {
      return "/adminonly/useradmin/usagestats";
    } else if (this.props.companyId) {
      return "/adminonly/companyadmin/usagestats";
    } else {
      return "/account/usagestats";
    }
  },
  enrichUrl: function (url) {
    var result = url + "/";
    if (this.props.userId) {
      result += this.props.userId;
    } else if (this.props.companyId) {
      result += this.props.companyId;
    } else {
      result += "json";
    }

    if (this.props.withCompany) {
      result += "?withCompany=true";
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
          <StatsTableView
            ref="tableDays"
            url={this.urlForScope(SCOPE_DAYS)}
            withCompany={this.props.withCompany}
          />
        </div>

        <h2>{localization.account.stats.last6months}</h2>
        <div className="jsmonthtable">
          <StatsTableView
            ref="tableMonths"
            url={this.urlForScope(SCOPE_MONTHS)}
            withCompany={this.props.withCompany}
          />
        </div>
      </div>
    );
  }
});

module.exports = StatsView;
