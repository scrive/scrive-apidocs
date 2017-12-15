var jQuery = require("jquery");
var React = require("react");
var underscore = require("underscore");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var ConfirmationsWithEmails = require("../../js/confirmationsWithEmails.js");
var EmailModal = require("../../scripts/common/email_modal");
var Modal = require("../../scripts/common/modal");

describe("common/email_modal", function () {
  var container = null;

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  describe("EmailContentView", function () {
    var renderComponent = function (props, componentClass) {
      container = document.createElement('div');

      var defaultProps = {
        content: jQuery("<div class='mail-content'>MAIL</div>"),
        editing: false,
        editWidth: 300,
        emailReady: true
      };

      var actualProps = underscore.extendOwn({}, defaultProps, props || {});
      var component = React.render(
        React.createElement(
          componentClass || EmailModal.EmailContentView, actualProps
        ),
        container
      );

      return component;
    };

    it("should inject the content when it mounts", function () {
      var component = renderComponent();

      var content = jQuery(".mail-content", container);
      assert.equal(content.text(), "MAIL");
    });

    it("should inject the content when it updates", function () {
      var component = renderComponent();
      sinon.stub(component, "injectContent");

      component.componentDidUpdate();
      assert.isTrue(component.injectContent.called);
    });

    it("should remove the custom custom message editor when it unmounts", function () {
      var textarea = jQuery("<textarea />");
      sinon.stub(textarea, "remove");

      var component = renderComponent();
      component._textarea = textarea;

      React.unmountComponentAtNode(container);
      container = null;

      assert.isTrue(textarea.remove.called);
      assert.isNull(component._textarea);
    });

    describe("customMessage", function () {
      it("should return an empty string if there's no custom message editor", function () {
        var component = renderComponent();

        var result = component.customMessage();
        assert.equal(result, "");
      });

      it("should return the custom message editor's value", function () {
        var textarea = jQuery("<textarea />");
        textarea.val("spam");

        var component = renderComponent();
        component._textarea = textarea;

        var result = component.customMessage();
        assert.equal(result, "spam");
      });
    });

    describe("injectContent", function () {
      var Wrapper = React.createClass({
        getInitialState: function () {
          return {
            content: this.props.content
          }
        },
        render: function () {
          return React.createElement(
            EmailModal.EmailContentView,
            {
              ref: "emailContentView",
              content: this.state.content,
              editing: this.props.editing,
              editWidth: this.props.editWidth,
              emailReady: this.props.emailReady
            }
          )
        }
      });

      it("should replace the current content with new", function () {
        var component = renderComponent(undefined, Wrapper);
        component.setState({
          content: jQuery("<div class='mail-content'>NEW</div>")
        });

        var content = jQuery(".mail-content", container);
        assert.equal(content.text(), "NEW");
      });

      it("should render the custom message editor if it's editing", function () {
        var content = jQuery(
          "<div class='mail-content'><div class='editable' /></div>"
        );

        var component = renderComponent(
          {content: content, editing: true}, Wrapper
        );

        var textarea = jQuery("textarea", container);
        assert.isNotNull(component.refs.emailContentView._textarea);
        assert.equal(textarea.css("width"), "300px");
      });
    });

    describe("render", function () {
      it("should render as loading when mail isn't ready", function () {
        var component = renderComponent({emailReady: false});
        assert.isTrue(component.getDOMNode().classList.contains("loadingMail"));
      });

      it("should not render as loading when mail is ready", function () {
        var component = renderComponent({emailReady: true});
        assert.isFalse(
          component.getDOMNode().classList.contains("loadingMail")
        );
      });
    });
  });

  describe("EmailView", function () {
    var document_ = null;
    var mail = null;
    var server = null;
    var signatory = null;

    var renderComponent = function (props, componentClass) {
      container = document.createElement('div');

      var defaultProps = {
        active: false,
        document: document_,
        signatory: signatory,
        title: "Spam",
        type: "invite",
        onAccept: sinon.stub(),
        onClose: sinon.stub()
      };

      var actualProps = underscore.extendOwn({}, defaultProps, props || {});
      var component = React.render(
        React.createElement(
          componentClass || EmailModal.EmailModal, actualProps
        ),
        container
      );

      return component;
    };

    before(function () {
      server = backend.createServer();
    });

    beforeEach(function (done) {
      util.createDocument(function (doc) {
        document_ = doc;
        signatory = document_.signatories()[0];

        mail = new ConfirmationsWithEmails.Mail({
          document: document_
        });
        sinon.stub(mail, "fetch");
        sinon.stub(mail, "on");
        sinon.stub(mail, "off");

        sinon.stub(ConfirmationsWithEmails, "Mail").returns(mail);

        done();
      });
    });

    afterEach(function () {
      ConfirmationsWithEmails.Mail.restore();
    });

    after(function () {
      server.restore();
    });

    describe("componentWillReceiveProps", function () {
      it("should remove change listener on old model when active changes", function () {
        var component = renderComponent({active: true});
        component._mail = mail;

        component.componentWillReceiveProps({active: false});
        assert.isTrue(mail.off.calledWith("change", component.onMailChange));
      });

      it("should create new mail model when it becomes active", function () {
        var component = renderComponent();

        component.componentWillReceiveProps({active: true});
        assert.isTrue(ConfirmationsWithEmails.Mail.calledWithNew());
        assert.isTrue(ConfirmationsWithEmails.Mail.calledWith({
          document: component.props.document,
          signatory: component.props.signatory,
          editWidth: component.props.editWidth,
          type: component.props.type
        }));
        assert.isTrue(mail.on.calledWith("change", component.onMailChange));
        assert.isTrue(mail.fetch.called);

        var fetchOptions = mail.fetch.firstCall.args[0];
        assert.equal(fetchOptions.data.mailtype, component.props.type);
        assert.isTrue(fetchOptions.processData);
        assert.isFalse(fetchOptions.cache);
      });
    });

    it("should remove change listener on old model when it unmounts", function () {
      var component = renderComponent({active: true});
      component._mail = mail;

      component.componentWillReceiveProps({active: false});
      assert.isTrue(mail.off.calledWith("change", component.onMailChange));
    });

    it("should call the onAccept callback when the accept button is clicked", function () {
      var component = renderComponent();
      component._mail = mail;
      component.forceUpdate();

      sinon.stub(component.refs.emailContentView, "customMessage").returns(
        "spam"
      );

      component.onAcceptButtonClick();
      assert.isTrue(component.props.onAccept.calledWith("spam"));
    });

    it("should make the mail editable when the edit button is clicked", function () {
      sinon.stub(mail, "makeEditable");

      var component = renderComponent();
      component._mail = mail;

      component.onEditButtonClick();
      assert.isTrue(mail.makeEditable.called);
    });

    it("should update when the mail model changes", function () {
      var component = renderComponent();
      sinon.stub(component, "forceUpdate");

      component.onMailChange();
      assert.isTrue(component.forceUpdate.called);
    });

    describe("render", function () {
      it("should configure and render the modal container", function () {
        var component = renderComponent({
          width: 500,
          onShow: sinon.stub(),
          onHide: sinon.stub()
        });

        assert.equal(
          component.refs.modal.props.active, component.props.active
        );
        assert.equal(
          component.refs.modal.props.width, component.props.width
        );
        assert.equal(
          component.refs.modal.props.onShow, component.props.onShow
        );
        assert.equal(
          component.refs.modal.props.onHide, component.props.onHide
        );
      });

      it("should configure and render the modal header", function () {
        var component = renderComponent();

        assert.equal(
          component.refs.modalHeader.props.title, component.props.title
        );
        assert.isTrue(component.refs.modalHeader.props.showClose);
        assert.equal(
          component.refs.modalHeader.props.onClose, component.props.onClose
        );
      });

      it("should not render the email content view if there's no mail model", function () {
        var component = renderComponent();
        assert.isUndefined(component.refs.emailContentView);
      });

      it("should configure and render the email content view", function () {
        var component = renderComponent();
        component._mail = mail;

        component.forceUpdate();

        assert.isDefined(component.refs.emailContentView);
        assert.equal(
          component.refs.emailContentView.props.content, mail.content()
        );
        assert.equal(
          component.refs.emailContentView.props.editing, mail.editable()
        );
        assert.equal(
          component.refs.emailContentView.props.editWidth, mail.editWidth()
        );
        assert.equal(
          component.refs.emailContentView.props.emailReady, mail.ready()
        );
      });

      it("should not render the cancel button if it can't be rejected", function () {
        var component = renderComponent({allowReject: false});
        assert.isUndefined(component.refs.cancelButton);
      });

      it("should configure and render the cancel button", function () {
        var component = renderComponent();
        assert.isDefined(component.refs.cancelButton);
        assert.equal(
          component.refs.cancelButton.props.onClick, component.props.onClose
        );
      });

      it("should not render the edit button if it can't be edited", function () {
        var component = renderComponent({allowEdit: false});
        assert.isUndefined(component.refs.editButton);
      });

      it("should not render the edit button if there's no mail model", function () {
        var component = renderComponent({allowEdit: true});
        assert.isUndefined(component.refs.editButton);
      });

      it("should not render the edit button if the mail model is editing", function () {
        sinon.stub(mail, "editable").returns(true);

        var component = renderComponent({allowEdit: true});
        component._mail = mail;

        component.forceUpdate();

        assert.isUndefined(component.refs.editButton);
      });

      it("should configure and render the edit button", function () {
        var component = renderComponent({allowEdit: true});
        component._mail = mail;

        component.forceUpdate();

        assert.isDefined(component.refs.editButton);
        assert.equal(
          component.refs.editButton.props.onClick, component.onEditButtonClick
        );
      });

      it("should configure and render the accept button", function () {
        var component = renderComponent();
        assert.isDefined(component.refs.acceptButton);
        assert.equal(
          component.refs.acceptButton.props.onClick,
          component.onAcceptButtonClick
        );
      });
    });
  });
});
