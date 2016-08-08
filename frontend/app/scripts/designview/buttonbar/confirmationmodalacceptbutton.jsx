var React = require("react");
var Spinner = require("spin.js");

var Button = require("../../common/button");

var SpinnerComponent = React.createClass({
  componentDidMount: function () {
    this.spinner = new Spinner({
      lines: 9,
      length: 3,
      width: 2,
      radius: 5,
      color: "#000000",
      speed: 1.5,
      trail: 74,
      shadow: false
    });
    this.spinner.spin();

    this.getDOMNode().appendChild(this.spinner.el);
  },
  componentWillUnmount: function () {
    this.getDOMNode.removeChild(this.spinner.el);
  },
  render: function () {
    return <div className="spinner-container design-view-modal-spinner" />;
  }
});

module.exports = React.createClass({
  propTypes: {
    onClick: React.PropTypes.func.isRequired,
    text: React.PropTypes.string.isRequired
  },
  getInitialState: function () {
    return {
      showSpinner: false
    };
  },
  onClick: function () {
    this.setState({showSpinner: true});
    this.props.onClick(this);
  },
  render: function () {
    return (
      <span className="float-right">
        { /* if */ (this.state.showSpinner) &&
          <SpinnerComponent />
        }
        <Button
          type="action"
          text={this.props.text}
          oneClick={true}
          onClick={this.onClick}
        />
      </span>
    );
  }
});
