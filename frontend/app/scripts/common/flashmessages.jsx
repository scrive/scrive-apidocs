var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");

var FlashMessageView = React.createClass({
  propTypes: {
    type: React.PropTypes.string.isRequired,
    content: React.PropTypes.string.isRequired
  },
  componentWillMount: function () {
    this._visible = false;
  },
  componentDidMount: function () {
    $(this.refs.body.getDOMNode()).append(this.props.content);

    this.hide();

    this.getDOMNode().addEventListener("transitionend", this.onTransitionEnd);

    this._showTimeout = window.setTimeout(this.show, 100);
    this._hideTimeout = window.setTimeout(this.hide, 10000);
  },
  componentWillUnmount: function () {
    this.getDOMNode().removeEventListener(
      "transitionend", this.onTransitionEnd
    );
  },
  hide: function () {
    window.clearTimeout(this._hideTimeout);
    this._visible = false;

    var $node = $(this.getDOMNode());
    $node.css("top", -1 * $node.height());
  },
  show: function () {
    window.clearTimeout(this._showTimeout);
    this._visible = true;

    $(this.getDOMNode()).css("top", 0);
  },
  onTransitionEnd: function () {
    if (!this._visible) {
      this.props.onHide();
    }
  },
  onCloseClick: function () {
    this.hide();
  },
  render: function () {
    var type = this.props.type;
    if (!(type == "success" || type == "error")) {
      type = "success";
    }

    var className = classNames("flash-content-wrapper", type);

    return (
      <div className={className}>
        <div className="flash-content">
          <div ref="body" className="flash-body" />
          <div
            className="flash-close"
            onClick={this.onCloseClick}
          >
            &times;
          </div>
        </div>
      </div>
    );
  }
});

module.exports = FlashMessageView;
