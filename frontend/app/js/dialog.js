/*
 * Screen blocking dialog
 *
 * Open with:
 * ScreenBlockingDialog.open({
 *   "header": some html/jquery object,
 *   "subheader": some html/jquery object,
 *   "content": some html/jquery object
 * });
 *
 * Close with: ScreenBlockingDialog.close();
 */

var React = require("react");
var $ = require("jquery");

var ReactDialog = require("../scripts/common/dialog");

var currentContainer = null;
var currentDialog = null;

var DialogContentWrapper = React.createClass({
  propTypes: {
    content: React.PropTypes.object
  },
  componentDidMount: function () {
    $(React.findDOMNode(this)).append(this.props.content);
  },
  render: function () {
    return React.createElement("div", {});
  }
});

var DialogWrapper = React.createClass({
  propTypes: {
    content: React.PropTypes.object,
    header: React.PropTypes.object,
    subheader: React.PropTypes.object,
    onHide: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      active: false
    }
  },
  componentDidMount: function () {
    this.setState({active: true});
  },
  render: function () {
    var dialogChildren = [];
    if (this.props.header) {
      dialogChildren.push(
        React.createElement(
          ReactDialog.Header, {key: 0},
          React.createElement(
            DialogContentWrapper, {content: this.props.header}
          )
        )
      );
    }

    if (this.props.subheader) {
      dialogChildren.push(
        React.createElement(
          ReactDialog.SubHeader, {key: 1},
          React.createElement(
            DialogContentWrapper, {content: this.props.subheader}
          )
        )
      );
    }

    if (this.props.content) {
      dialogChildren.push(
        React.createElement(
          ReactDialog.Content, {key: 2},
          React.createElement(
            DialogContentWrapper, {content: this.props.content}
          )
        )
      );
    }

    return React.createElement(
      ReactDialog.Dialog,
      {
        active: this.state.active,
        onHide: this.props.onHide
      },
      dialogChildren
    );
  }
});

var onDialogHide = function () {
  if (currentContainer) {
    React.unmountComponentAtNode(currentContainer);
    document.body.removeChild(currentContainer);
    currentContainer = null;
    currentDialog = null;
  }
};

var open = function (config) {
  if (!currentContainer) {
    currentContainer = document.createElement("div");
    $("body").append(currentContainer);

    currentDialog = React.render(
      React.createElement(
        DialogWrapper,
        {
          content: config.content,
          header: config.header,
          subheader: config.subheader,
          onHide: onDialogHide
        }
      ),
      currentContainer
    );
  }
};

var close = function () {
  if (currentDialog) {
    currentDialog.setState({active: false});
  }
};

module.exports = {
  ScreenBlockingDialog: {
    open: open,
    close: close
  }
};
