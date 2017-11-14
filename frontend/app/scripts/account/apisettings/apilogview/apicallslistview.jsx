var classNames = require("classnames");
var React = require("react");

var APICallsCollection = require("./apicallscollection");
var APICallsListItemView = require("./apicallslistitemview");

var APICallsListView = React.createClass({
  mixins: [React.addons.PureComponentMixin],
  propTypes: {
    calls: React.PropTypes.instanceOf(APICallsCollection).isRequired,
    hidden: React.PropTypes.bool,
    onSelect: React.PropTypes.func.isRequired
  },
  render: function () {
    var self = this;

    var className = classNames("list-container", "apicallslistview", {
      hidden: this.props.hidden
    });

    return (
      <div className={className}>
        <div className="table">
          <table>
            <thead>
              <tr>
                <th>
                  <span>{"Date & Time (UTC)"}</span>
                </th>
                <th>
                  <span>{"Request"}</span>
                </th>
                <th>
                  <span>{"HTTP Status"}</span>
                </th>
              </tr>
            </thead>
            <tbody>
              {this.props.calls.map(function (call, index) {
                return (
                  <APICallsListItemView
                    key={call.cid}
                    call={call}
                    onSelect={self.props.onSelect}
                  />
                );
              })}
            </tbody>
          </table>
        </div>
      </div>
    );
  }
});

module.exports = APICallsListView;
