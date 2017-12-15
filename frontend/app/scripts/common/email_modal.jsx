var Backbone = require("backbone");
var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var Button = require("./button");
var ConfirmationsWithEmails = require("../../js/confirmationsWithEmails.js");
var DocumentModel = require("../../js/documents.js").Document;
var SignatoryModel = require("../../js/signatories.js").Signatory;
var Modal = require("./modal");

var EmailContentView = React.createClass({
  propTypes: {
    content: React.PropTypes.object.isRequired,
    editing: React.PropTypes.bool.isRequired,
    editWidth: React.PropTypes.number.isRequired,
    emailReady: React.PropTypes.bool.isRequired
  },
  componentWillMount: function () {
    this._textarea = null;
  },
  componentDidMount: function () {
    this.injectContent();
  },
  componentDidUpdate: function () {
    this.injectContent();
  },
  componentWillUnmount: function () {
    if (this._textarea) {
      this._textarea.remove();
    }

    this._textarea = null;
  },
  customMessage: function () {
    if (this._textarea) {
      return this._textarea.val();
    }

    return "";
  },
  injectContent: function () {
    this._textarea = null;

    var $bodyNode = $(this.getDOMNode());
    $bodyNode.empty();

    var content = $(this.props.content).clone();
    if (this.props.editing) {
      this._textarea = $("<textarea />");
      this._textarea.css("width", this.props.editWidth + "px");

      var wrapper = $("<div style='margin-bottom:12px;'/>");
      wrapper.append(this._textarea);

      $(".editable", content).replaceWith(wrapper);
    }

    $(this.getDOMNode()).append(content);
  },
  render: function () {
    var className = classNames({
      loadingMail: !this.props.emailReady
    });

    return (
      <div className={className} />
    );
  }
});

var EmailModal = React.createClass({
  propTypes: {
    acceptText: React.PropTypes.string,
    acceptType: React.PropTypes.string,
    active: React.PropTypes.bool.isRequired,
    allowEdit: React.PropTypes.bool,
    allowReject: React.PropTypes.bool,
    cancelText: React.PropTypes.string,
    document: React.PropTypes.instanceOf(DocumentModel).isRequired,
    editText: React.PropTypes.string,
    editWidth: React.PropTypes.number,
    signatory: React.PropTypes.instanceOf(SignatoryModel),
    title: React.PropTypes.string.isRequired,
    type: React.PropTypes.string.isRequired,
    width: React.PropTypes.number,
    onAccept: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired,
    onShow: React.PropTypes.func,
    onHide: React.PropTypes.func
  },
  componentWillMount: function () {
    this._mail = null;
  },
  componentWillReceiveProps: function (nextProps) {
    if (nextProps.active != this.props.active) {
      if (this._mail) {
        this._mail.off("change", this.onMailChange);
      }

      if (nextProps.active) {
        this._mail = new ConfirmationsWithEmails.Mail({
          document: this.props.document,
          signatory: this.props.signatory,
          editWidth: this.props.editWidth,
          type: this.props.type
        });
        this._mail.on("change", this.onMailChange);

        this._mail.fetch({
          data: {mailtype: this.props.type},
          processData: true,
          cache: false
        });
      }
    }
  },
  componentWillUnmount: function () {
    if (this._mail) {
      this._mail.off("change", this.onMailChange);
    }
  },
  onAcceptButtonClick: function () {
    this.props.onAccept(this.refs.emailContentView.customMessage());
  },
  onEditButtonClick: function () {
    this._mail.makeEditable();
  },
  onMailChange: function () {
    this.forceUpdate();
  },
  render: function () {
    var allowEdit = (
      _.isUndefined(this.props.allowEdit) ? true : this.props.allowEdit
    );

    var allowReject = (
      _.isUndefined(this.props.allowReject) ? true : this.props.allowReject
    );

    return (
      <Modal.Container
        ref="modal"
        active={this.props.active}
        width={this.props.width}
        onShow={this.props.onShow}
        onHide={this.props.onHide}
      >
        <Modal.Header
          ref="modalHeader"
          title={this.props.title}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          {this._mail &&
            <EmailContentView
              ref="emailContentView"
              content={this._mail.content()}
              editing={this._mail.editable()}
              editWidth={this._mail.editWidth()}
              emailReady={this._mail.ready()}
            />
          }
        </Modal.Content>
        <Modal.Footer>
          {allowReject &&
            <Modal.CancelButton
              ref="cancelButton"
              text={this.props.cancelText || localization.cancel}
              onClick={this.props.onClose}
            />
          }
          {allowEdit && (this._mail && !this._mail.editable()) &&
            <Button
              ref="editButton"
              className="float-left"
              style={{marginLeft: 15}}
              text={this.props.editText || "Edit"}
              onClick={this.onEditButtonClick}
            />
          }
          <Modal.AcceptButton
            ref="acceptButton"
            text={this.props.acceptText || localization.ok}
            type={this.props.acceptType}
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = {
  EmailContentView: EmailContentView,
  EmailModal: EmailModal
};
