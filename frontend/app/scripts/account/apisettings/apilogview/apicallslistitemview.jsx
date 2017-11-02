var classNames = require("classnames");
var React = require("react");

var APICallModel = require("./apicallmodel");
var utils = require("./utils");

var APICallsListItemView = React.createClass({
  mixins: [React.addons.PureComponentMixin],
  propTypes: {
    call: React.PropTypes.instanceOf(APICallModel).isRequired,
    onSelect: React.PropTypes.func.isRequired
  },
  onSelect: function () {
    this.props.onSelect(this.props.call);
  },
  render: function () {
    var statusClassName = classNames(
      "request-status", utils.statusClassName(this.props.call.isSuccessful())
    );

    return (
      <tr onClick={this.onSelect}>
        <td className="row">{this.props.call.displayTime()}</td>
        <td className="row">{this.props.call.get("requestURI")}</td>
        <td className="row">
          <span className={statusClassName}>
            {this.props.call.get("responseCode")}
          </span>
        </td>
      </tr>
    );
  }
});

module.exports = APICallsListItemView;
