var React = require("react");
var classNames = require("classnames");
var $ = require("jquery");
var _ = require("underscore");

import vars_ from '!less-vars-loader!../../less/branding/modal.less';
import {toLessInteropLoader} from './less_utils.jsx';
const vars = toLessInteropLoader(vars_);

var onTransitionEndDelay = parseInt(vars.transitionDuration, 10) + 100;
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

var Container = exports.Container = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    width: React.PropTypes.number,
    onShow: React.PropTypes.func,
    onHide: React.PropTypes.func,
    onClose: React.PropTypes.func,
    id: React.PropTypes.string,
    marginTop: React.PropTypes.number
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

  componentDidUpdate: function (prevProps) {
    if (prevProps.active != this.props.active) {
      this._onTransitionEndTimeout = window.setTimeout(
        this.onTransitionEnd, onTransitionEndDelay
      );
    }
  },

  onTransitionEnd: function () {
    if (this.isMounted()) {

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

  onInnerClick: function (event) {
    event.stopPropagation();
  },

  onClose: function (event) {
    if (this.props.onClose) {
      this.props.onClose(event);
    }
  },

  render: function () {
    var overlayClassName = classNames("modal", this.props.className, {
      "active": this.props.active
    });

    var minimalMargin = 20;
    var computedWidth = $(window).width() - 2 * minimalMargin;
    var width = Math.min(this.props.width || 640, computedWidth);
    var marginTop = this.props.marginTop || 50;
    var left = Math.floor(($(window).width() - width) / 2);

    var containerStyle = {
      marginLeft: Math.max(left, minimalMargin) + $(window).scrollLeft(),
      marginTop: marginTop + $(window).scrollTop(),
      marginBottom: 50,
      width: width
    };

    var minHeight = $(document).height();

    return (
      <Portal>
        <div className={overlayClassName} style={{minHeight: minHeight,
                                                  display: this.props.active ? "block" : "none"}}
             id={this.props.id} onClick={this.onClose}>

          <div className="modal-container" style={containerStyle}>
            <div onClick={this.onInnerClick}>
              {this.props.children}
            </div>
          </div>
        </div>
      </Portal>
    );
  }
});

var Header = exports.Header = React.createClass({
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
          <a className="modal-close-wrapper" onClick={this.props.onClose}>
            <span className="modal-close" />
          </a>
        }
      </div>
    );
  }
});

var Content = exports.Content = React.createClass({
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

var Footer = exports.Footer = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  render: function () {
    return (
      <div className="modal-footer">
        {this.props.children}
      </div>
    );
  }
});

var CancelButton = exports.CancelButton = React.createClass({
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

var AcceptButton = exports.AcceptButton = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    text: React.PropTypes.string,
    onClick: React.PropTypes.func,
    type: React.PropTypes.string,
    className: React.PropTypes.string
  },
  render: function () {
    var className = classNames("float-right", this.props.className);
    return (
      <Button
        className={className}
        type={(this.props.type) ? this.props.type : "action"}
        text={(this.props.text) ? this.props.text : localization.ok}
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

exports.InfoBox = React.createClass({
  propTypes: {
    active: React.PropTypes.bool,
    title: React.PropTypes.string,
    onClose: React.PropTypes.func
  },

  render: function () {
    return (
      <Container
        active={this.props.active}
        onHide={this.props.onClose}
        onClose={this.props.onClose}
      >
        <Header
          onClose={this.props.onClose}
          title={this.props.title}
          showClose={true}
        />

        <Content>{this.props.children}</Content>

        <Footer>
          <AcceptButton
            onClick={this.props.onClose}
            text={localization.ok}
          />
        </Footer>
      </Container>
    );
  }
});
