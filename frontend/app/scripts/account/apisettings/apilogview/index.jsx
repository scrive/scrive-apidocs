var React = require("react");
var _ = require("underscore");

var APICallsCollection = require("./apicallscollection");
var APICallsListView = require("./apicallslistview");
var APICallView = require("./apicallview").APICallView;

var APILogView = React.createClass({
  propTypes: {
    loadLater: React.PropTypes.bool
  },
  getInitialState: function () {
    return {
      loading: false,
      selectedCall: null
    };
  },
  componentWillMount: function () {
    this.collection = new APICallsCollection();
  },
  componentDidMount: function () {
    if (window.location.hash == "#api-call" && !this.state.selectedCall) {
      window.location.hash = "api-log";
    }

    window.addEventListener("hashchange", this.onWindowHashChange);

    if (this.props.loadLater === false) {
      this.reload();
    }
  },
  componentWillUnmount: function () {
    window.removeEventListener("hashchange", this.onWindowHashChange);
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevState.selectedCall != this.state.selectedCall) {
      if (this.state.selectedCall) {
        window.location.hash = "#api-call";
      } else {
        window.location.hash = "#api-log";
      }
    }
  },
  reload: function () {
    this.setState({loading: true});
    this.collection.fetch({reset: true, success: this.onCollectionFetch});
  },
  onCollectionFetch: function () {
    this.setState({
      loading: false,
      selectedCall: null
    });
  },
  onCallListSelect: function (call) {
    this.setState({selectedCall: call});
  },
  onCallViewClose: function () {
    this.setState({selectedCall: null});
  },
  onWindowHashChange: function () {
    if (window.location.hash == "#api-log") {
      if (this.state.selectedCall) {
        this.setState({selectedCall: null});
      }
    } else if (window.location.hash == "#api-call") {
      if (!this.state.selectedCall) {
        window.location.hash = "api-log";
      }
    }
  },
  render: function () {
    var hasSelectedCall = !_.isNull(this.state.selectedCall);

    return (
      <div className="apilogview">
        <APICallsListView
          calls={this.collection}
          hidden={hasSelectedCall}
          onSelect={this.onCallListSelect}
        />

        {hasSelectedCall &&
          <APICallView
            call={this.state.selectedCall}
            onClose={this.onCallViewClose}
          />
        }
      </div>
    );
  }
});

module.exports = APILogView;
