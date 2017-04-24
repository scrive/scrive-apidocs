var React = require("react");

var Track = require("./track");

var TrackingView = React.createClass({
  propTypes: {
    mixpanelSubcontext: React.PropTypes.string,
    trackEvent: React.PropTypes.string
  },
  componentDidMount: function () {
    if (this.props.mixpanelSubcontext) {
      mixpanel.register({Subcontext: this.props.mixpanelSubcontext});
    }

    if (this.props.trackEvent) {
      Track.track(this.props.trackEvent);
    }
  },
  render: function () {
    return this.props.children;
  }
});

module.exports = TrackingView;
