var React = require("react");
var $ = require("jquery");

var Modal = require("./modal");

var DIALOG_WIDTH = 650;

var Dialog = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onHide: React.PropTypes.func,
    id: React.PropTypes.string
  },

  getInitialState: function () {
    var id = "dialog-" + Math.floor(Math.random()*65536).toString();
    return {
      id: this.props.id ? this.props.id : id
    };
  },

  componentWillMount: function () {
    this._onWindowResizeTimeout = null;
  },

  componentDidMount: function () {
    window.addEventListener("resize", this.onWindowResize, false);
    this.fixPosition()
  },

  componentWillUnmount: function () {
    window.removeEventListener("resize", this.onWindowResize, false);

    this.clearWindowResizeTimeout();
  },

  componentWillReceiveProps: function (nextProps) {
    if (nextProps.active) {
      this.fixPosition()
    }
  },

  clearWindowResizeTimeout: function () {
    if (this._onWindowResizeTimeout) {
      window.clearTimeout(this._onWindowResizeTimeout);
      this._onWindowResizeTimeout = null;
    }
  },

  // this.getDOMNode(), React.findDOMNode(this) and ReactDOM.findDOMNode(this)
  // all return null because of Portal.
  getActualNode: function () {
    return document.querySelector("#" + this.state.id);
  },

  fixPosition: function () {
    var node = this.getActualNode();
    if(node) {
      var $container = $(".modal-container", node);
      var marginLeft = ($(window).width() - DIALOG_WIDTH) / 2 + $(window).scrollLeft();
      var width = $container.css("width");
      if (marginLeft < 0) {
        // must be a tiny window
        marginLeft = 0;
        width = "100%";
      }
      $container.css({
        marginLeft: marginLeft,
        marginTop: ($(window).height() - 200) / 2 + $(window).scrollTop(),
        width: width
      });
    }
  },

  onWindowResize: function () {
    if (!this._onWindowResizeTimeout) {
      this._onWindowResizeTimeout = window.setTimeout(
        this.onWindowResizeTimeout, 66
      );
    }
  },

  onWindowResizeTimeout: function () {
    this.clearWindowResizeTimeout();
    this.fixPosition();
  },

  render: function () {
    return (
      <Modal.Container
        ref="modal"
        active={this.props.active}
        className="screenblockingdialog"
        width={DIALOG_WIDTH}
        onHide={this.props.onHide}
        id={this.state.id}
      >
        <Modal.Content ref="modalContent">
          <center>
            {this.props.children}
          </center>
        </Modal.Content>
      </Modal.Container>
    );
  }
});

var Header = React.createClass({
  render: function () {
    return (
      <h4 className="screenblockingheader">{this.props.children}</h4>
    );
  }
});

var SubHeader = React.createClass({
  render: function () {
    return (
      <h4 className="screenblockingsubheader">{this.props.children}</h4>
    );
  }
});

var Content = React.createClass({
  render: function () {
    return (
      <div className="screenblockingcontent">{this.props.children}</div>
    );
  }
});

module.exports = {
  Dialog: Dialog,
  Header: Header,
  SubHeader: SubHeader,
  Content: Content
};
