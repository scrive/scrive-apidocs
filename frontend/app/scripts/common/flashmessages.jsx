var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");

var FlashMessageView = React.createClass({
  propTypes: {
    type: React.PropTypes.string.isRequired,
    content: React.PropTypes.string.isRequired
  },
  getInitialState: function() {
    return {visible: false};
  },
  componentDidMount: function () {
    $(this.refs.body.getDOMNode()).append(this.props.content);
    this.getDOMNode().addEventListener("transitionend", this.onTransitionEnd);
    this._showTimeout = window.setTimeout(this.show, 100);
    this._hideTimeout = window.setTimeout(this.hide, 10000);
  },
  componentWillUnmount: function () {
    this.getDOMNode().removeEventListener(
      "transitionend", this.onTransitionEnd
    );
    window.clearTimeout(this._hideTimeout);
    window.clearTimeout(this._showTimeout);
  },
  hide: function () {
    window.clearTimeout(this._hideTimeout);
    this.setState({visible : false});
  },
  show: function () {
    window.clearTimeout(this._showTimeout);
    this.setState({visible : true});
  },
  onTransitionEnd: function () {
    if (!this.state.visible) {
      this.props.onHide();
    }
  },
  render: function () {
    return (
      <div className={classNames("flash-content-wrapper", this.props.type || "success")}
           style={{display: this.state.visible ? "block" : "none"}}
        >
        <div className="flash-content">
          <div ref="body" className="flash-body" />
          <div className="flash-close" onClick={this.hide}>
            &times;
          </div>
        </div>
      </div>
    );
  }
});

module.exports = FlashMessageView;
