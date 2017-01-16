var React = require("react");
var classNames = require("classnames");
var $ = require("jquery");
var _ = require("underscore");

var transitionDuration = require("../../less/branding/modal.less").transitionDuration;
var onTransitionEndDelay = parseInt(transitionDuration, 10) + 100;
exports.onTransitionEndDelay = onTransitionEndDelay;

var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var Button = require("./button");

var Portal = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  componentWillMount: function () {
    this._portal = document.createElement("div");
    document.body.appendChild(this._portal);
  },
  renderPortal: function () {
    return <div>{this.props.children}</div>;
  },
  componentDidMount: function () {
    React.render(this.renderPortal(), this._portal);
  },
  componentDidUpdate: function () {
    React.render(this.renderPortal(), this._portal);
  },
  componentWillUnmount: function () {
    React.unmountComponentAtNode(this._portal);
    try {
      document.body.removeChild(this._portal);
    } catch (err) {
      // Apparently, the portal node has already been removed from the DOM.
      // This shouldn't happen in the browser but may happen in tests.
    }
  },
  render: function () {
    return null;
  }
});

exports.Container = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    width: React.PropTypes.number,
    onShow: React.PropTypes.func,
    onHide: React.PropTypes.func
  },
  getInitialState: function () {
    return {
      height: 0
    };
  },
  componentWillMount: function () {
    this._onTransitionEndTimeout = null;
  },
  componentWillUnmount: function () {
    window.clearTimeout(this._onTransitionEndTimeout);
  },
  componentWillReceiveProps: function (nextProps) {
    if (nextProps.active) {
      this.updateOverlayHeight(nextProps.active);
    }
  },
  componentDidUpdate: function (prevProps) {
    if (prevProps.active != this.props.active) {
      this._onTransitionEndTimeout = window.setTimeout(
        this.onTransitionEnd, onTransitionEndDelay
      );
    }
  },
  onTransitionEnd: function () {
    if (this.isMounted()) {
      this.updateOverlayHeight(this.props.active);

      if (this.props.active) {
        if (_.isFunction(this.props.onShow)) {
          this.props.onShow();
        }
      } else {
        if (_.isFunction(this.props.onHide)) {
          this.props.onHide();
        }
      }
    }
  },
  updateOverlayHeight: function (active) {
    if (active) {
      this.setState({height: $(document).height()});
    } else {
      this.setState({height: 0});
    }
  },
  render: function () {
    var overlayClassName = classNames("modal", this.props.className, {
      "active": this.props.active
    });

    var width = this.props.width || (BrowserInfo.isSmallScreen() ? 980 : 640);
    var left = Math.floor(($(window).width() - width) / 2);

    var containerStyle = {
      left: 0,
      marginLeft: Math.max(left, 20),
      marginTop: 50,
      top: $(window).scrollTop(),
      width: width
    };

    if (this.state.height == 0) {
      containerStyle.top = 0;
    }

    return (
      <Portal>
        <div className={overlayClassName} style={{height: this.state.height}}>
          <div className="modal-container" style={containerStyle}>
            {this.props.children}
          </div>
        </div>
      </Portal>
    );
  }
});

exports.Header = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    title: React.PropTypes.string,
    showClose: React.PropTypes.bool,
    onClose: React.PropTypes.func
  },
  render: function () {
    var titleClassName = classNames("modal-title", {
      "small-device": BrowserInfo.isSmallScreen()
    });

    return (
      <div className="modal-header">
        <div className="modal-header-inner">
          <div className={titleClassName}>
            {this.props.title}
          </div>
        </div>
        {this.props.showClose &&
          <a className="modal-close" onClick={this.props.onClose} />
        }
      </div>
    );
  }
});

exports.Content = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  render: function () {
    return (
      <div className="modal-body">
        <div className="modal-content">
          <div className="body">
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
});

exports.Footer = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  render: function () {
    return (
      <div className="modal-footer">
        {this.props.children}
      </div>
    );
  }
});

exports.CancelButton = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    text: React.PropTypes.string,
    onClick: React.PropTypes.func
  },
  render: function () {
    return (
      <label className="close float-left" onClick={this.props.onClick}>
        {(this.props.text) ? this.props.text : localization.cancel}
      </label>
    );
  }
});

exports.AcceptButton = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    text: React.PropTypes.string,
    oneClick: React.PropTypes.bool,
    onClick: React.PropTypes.func,
    type: React.PropTypes.string
  },
  render: function () {
    return (
      <Button
        className="float-right"
        type={(this.props.type) ? this.props.type : "action"}
        text={(this.props.text) ? this.props.text : localization.ok}
        oneClick={this.props.oneClick}
        onClick={this.props.onClick}
      />
    );
  }
});

exports.ExtraButtons = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    marginRight: React.PropTypes.number
  },
  render: function () {
    var marginRight = (this.props.marginRight) ? this.props.marginRight : 20;
    return (
      <div className="float-right" style={{marginRight: marginRight}}>
        {this.props.children}
      </div>
    );
  }
});
