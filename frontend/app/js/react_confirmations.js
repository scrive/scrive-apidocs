var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var Modal = require("../scripts/common/modal.jsx");

var ConfirmationContentWrapper = React.createClass({
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

var ConfirmationModal = React.createClass({
  propTypes: {
    title: React.PropTypes.string,
    onAccept: React.PropTypes.func,
    onClose: React.PropTypes.func,
    showAccept: React.PropTypes.bool,
    showCancel: React.PropTypes.bool,
    showClose: React.PropTypes.bool,
    showExtraButtons: React.PropTypes.bool,
    width: React.PropTypes.number,
    clearOnAccept: React.PropTypes.bool,
    content: React.PropTypes.object,
    acceptText: React.PropTypes.string,
    cancelText: React.PropTypes.string,
    onTransitionEnd: React.PropTypes.func,
    extraButtons: React.PropTypes.object
  },
  getInitialState: function () {
    return {
      visible: false,
      title: this.props.title,
      showAccept: this.props.showAccept,
      showCancel: this.props.showCancel,
      showClose: this.props.showClose,
      showExtraButtons: this.props.showExtraButtons,
      width: this.props.width,
      acceptText: this.props.acceptText,
      cancelText: this.props.cancelText
    };
  },
  componentWillMount: function () {
    this._onTransitionEndTimeout = null;
  },
  componentWillUnmount: function () {
    window.clearTimeout(this._onTransitionEndTimeout);
  },
  componentDidUpdate: function () {
    var self = this;

    this._onTransitionEndTimeout = window.setTimeout(
      function () {
        self.props.onTransitionEnd();
      },
      Modal.onTransitionEndDelay
    );
  },
  onClose: function () {
    this.setState({visible: false});

    if (_.isFunction(this.props.onClose)) {
      this.props.onClose();
    }
  },
  onAccept: function () {
    if (this.props.clearOnAccept === true) {
      this.setState({visible: false});
    }

    if (_.isFunction(this.props.onAccept)) {
      this.props.onAccept();
    }
  },
  fixOverlay: function () {
    var height = $(document).height();
    $("div.modal", this.getDOMNode()).height(height);
  },
  render: function () {
    var footerChildren = [];

    if (this.state.showCancel) {
      footerChildren.push(
        React.createElement(
          Modal.CancelButton,
          {
            text: this.state.cancelText,
            key: 0,
            onClick: this.onClose
          }
        )
      );
    }

    if (this.props.extraButtons && this.state.showExtraButtons) {
      footerChildren.push(
        React.createElement(Modal.ExtraButtons, {}, this.props.extraButtons)
      );
    }

    if (this.state.showAccept) {
      footerChildren.push(
        React.createElement(
          Modal.AcceptButton,
          {
            text: this.state.acceptText,
            key: 1,
            onClick: this.onAccept
          }
        )
      );
    }

    return React.createElement(
      Modal.Container,
      {active: this.state.visible, width: this.state.width},
      [
        React.createElement(
          Modal.Header,
          {
            key: 0,
            title: this.state.title,
            showClose: this.state.showClose,
            onClose: this.onClose
          }
        ),
        React.createElement(
          Modal.Content, {key: 1},
          React.createElement(
            ConfirmationContentWrapper, {content: this.props.content}
          )
        ),
        React.createElement(
          Modal.Footer, {key: 2}, footerChildren
        )
      ]
    );
  }
});

module.exports = function (config) {
  var onModalTransitionEnd = function () {
    if (modal.state.visible == false) {
      destroyModal();
    }
  };

  var modalProps = {
    title: config.title,
    onAccept: config.onAccept,
    onClose: config.onReject,
    acceptText: config.acceptText,
    cancelText: config.rejectText,
    showAccept: (typeof config.acceptVisible === "undefined") ? true : config.acceptVisible,
    showCancel: (typeof config.showCancel === "undefined") ? true : config.showCancel,
    showClose: (typeof config.showClose === "undefined") ? true : config.showClose,
    showExtraButtons: config.extraButtonsVisible,
    clearOnAccept: (typeof config.clearOnAccept === "undefined") ? false : config.clearOnAccept,
    width: config.width,
    content: config.content,
    onTransitionEnd: onModalTransitionEnd,
    extraButtons: config.extraButtons
  };

  var modalEl = document.createElement("div");
  var modal = React.render(
    React.createElement(ConfirmationModal, modalProps), modalEl
  );

  window.setTimeout(
    function () {
      if (modal.isMounted()) {
        modal.setState({visible: true});
      }
    },
    100
  );

  var destroyModal = function () {
    React.unmountComponentAtNode(modalEl);
  };

  return {
    clear: function () {
      destroyModal();
    },
    close: function () {
      modal.setState({visible: false});
    },
    fixOverlay: function () {
      modal.fixOverlay();
    },
    reject: function () {
      destroyModal();
    },
    hideAccept: function () {
      modal.setState({showAccept: false});
    },
    showAccept: function () {
      modal.setState({showAccept: true});
    },
    hideCancel: function () {
      modal.setState({showCancel: false});
    },
    showCancel: function () {
      modal.setState({showCancel: true});
    },
    hideClose: function () {
      modal.setState({showClose: false});
    },
    showClose: function () {
      modal.setState({showClose: true});
    },
    hideExtraButtons: function () {
      modal.setState({showExtraButtons: false});
    },
    showExtraButtons: function () {
      modal.setState({showExtraButtons: true});
    },
    setTitle: function (newTitle) {
      modal.setState({title: newTitle});
    },
    setWidth: function (newWidth) {
      modal.setState({width: newWidth});
    }
  };
};
