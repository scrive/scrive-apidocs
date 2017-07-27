var React = require("react");
var _ = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var Modal = require("../../../scripts/common/modal");
var SignatoryAttachment = require(
  "../../../js/signatoryattachment.js"
).SignatoryAttachment;
var SignatoryAttachmentsModal = require(
  "../../../scripts/designview/signatoryattachments/signatoryattachmentsmodal"
);
var SignatoryAttachmentsTable = require(
  "../../../scripts/designview/signatoryattachments/signatoryattachmentstable"
);

describe("designview/signatoryattachments/signatoryattachmentsmodal", function () {
  var container = null;
  var document_ = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var defaultProps = {
      active: false,
      document: document_,
      saveAndFlashMessageIfAlreadySaved: sinon.stub(),
      onClose: sinon.stub()
    };

    var actualProps = _.extendOwn({}, defaultProps, props || {});

    var component = React.render(
      React.createElement(SignatoryAttachmentsModal, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      signatory = document_.signatories()[1];

      signatory.addAttachment(
        new SignatoryAttachment({
          name: "spam",
          description: "eggs",
          required: true
        })
      );

      signatory.addAttachment(
        new SignatoryAttachment({
          name: "eggs",
          description: "spam",
          required: false
        })
      );

      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isArray(component.state.attachments);
  });

  it("should refresh attachments before becoming visible", function () {
    var component = renderComponent();
    sinon.stub(component, "refreshAttachments");

    component.componentWillReceiveProps({active: true});
    assert.isTrue(component.refreshAttachments.called);
  });

  it("should refresh attachments", function () {
    var signatory = document_.signatories()[1];

    var component = renderComponent();
    var oldAttachments = component.state.attachments;
    var signatoryAttachments = signatory.attachments();

    component.refreshAttachments();

    var attachments = component.state.attachments;
    assert.equal(attachments.length, signatoryAttachments.length);

    assert.equal(attachments[0].get("name"), signatoryAttachments[0].name());
    assert.equal(
      attachments[0].get("description"), signatoryAttachments[0].description()
    );
    assert.equal(attachments[0].get("signatory"), signatory);
    assert.equal(
      attachments[0].get("isRequired"), signatoryAttachments[0].isRequired()
    );

    assert.equal(attachments[1].get("name"), signatoryAttachments[1].name());
    assert.equal(
      attachments[1].get("description"), signatoryAttachments[1].description()
    );
    assert.equal(attachments[1].get("signatory"), signatory);
    assert.equal(
      attachments[1].get("isRequired"), signatoryAttachments[1].isRequired()
    );

    assert.notEqual(component.state.attachments, oldAttachments);
  });

  it("should clear attachments after becoming invisible", function () {
    var component = renderComponent();
    component.refreshAttachments();
    var oldAttachments = component.state.attachments;

    component.onHide();
    assert.equal(component.state.attachments.length, 0);
    assert.notEqual(component.state.attachments, oldAttachments);
  });
  
  it("should perform post-recall actions", function () {
    sinon.stub(document_, "trigger");

    var component = renderComponent();

    component.afterRecall();
    assert.isTrue(document_.trigger.calledWith("change:attachments"));
    assert.isTrue(component.props.saveAndFlashMessageIfAlreadySaved.called);
    assert.isTrue(component.props.onClose.called);
  });

  it("should recall the document after saving attachments", function () {
    sinon.stub(document_, "recall");

    var component = renderComponent();

    component.afterSave();
    assert.isTrue(document_.recall.calledWith(component.afterRecall));
  });

  it("should not save attachments when one of them isn't ready", function () {
    sinon.stub(document_, "save");
    sinon.stub(document_, "afterSave");

    var component = renderComponent();
    component.refreshAttachments();
    sinon.stub(component.state.attachments[0], "ready").returns(false);

    var result = component.onAcceptButtonClick();
    assert.isFalse(result);
    assert.isFalse(document_.save.called);
  });

  it("should not save attachments when there's duplicate name", function () {
    sinon.stub(document_, "save");
    sinon.stub(document_, "afterSave");

    var component = renderComponent();
    component.refreshAttachments();
    component.state.attachments[0].set("name", "duplicate");
    component.state.attachments[1].set("name", "duplicate");

    var result = component.onAcceptButtonClick();
    assert.isFalse(result);
    assert.isFalse(document_.save.called);
  });

  it("should clear attachments of all signatories before saving", function () {
    var author = document_.signatories()[0];

    sinon.stub(document_, "save");
    sinon.stub(document_, "afterSave");
    sinon.stub(author, "clearAttachments");
    sinon.stub(signatory, "clearAttachments");

    var component = renderComponent();

    var result = component.onAcceptButtonClick();
    assert.isTrue(author.clearAttachments.called);
    assert.isTrue(signatory.clearAttachments.called);
  });

  it("should add new attachments to signatories before saving", function () {
    sinon.stub(document_, "save");
    sinon.stub(document_, "afterSave");

    var oldSignatoryAttachments = signatory.attachments();

    var component = renderComponent();
    component.refreshAttachments();

    var attachments = component.state.attachments;
    attachments[0].set("name", "New Attachment 1");
    attachments[0].set("description", "New description 1");
    attachments[1].set("name", "New Attachment 2");
    attachments[1].set("description", "New description 2");

    var result = component.onAcceptButtonClick();
    assert.isTrue(result);

    var signatoryAttachments = signatory.attachments();
    assert.equal(signatoryAttachments.length, 2);
    assert.notEqual(oldSignatoryAttachments, signatoryAttachments);

    assert.equal(signatoryAttachments[0].name(), attachments[0].get("name"));
    assert.equal(
      signatoryAttachments[0].description(), attachments[0].get("description")
    );
    assert.equal(
      signatoryAttachments[0].isRequired(), attachments[0].get("isRequired")
    );

    assert.equal(signatoryAttachments[1].name(), attachments[1].get("name"));
    assert.equal(
      signatoryAttachments[1].description(), attachments[1].get("description")
    );
    assert.equal(
      signatoryAttachments[1].isRequired(), attachments[1].get("isRequired")
    );
  });

  it("should save new attachments", function () {
    sinon.stub(document_, "save");
    sinon.stub(document_, "afterSave");

    var component = renderComponent();
    component.refreshAttachments();

    var result = component.onAcceptButtonClick();
    assert.isTrue(result);

    assert.isTrue(document_.save.called);
    assert.isTrue(document_.afterSave.calledWith(component.afterSave));
  });

  it("should remove an attachment", function () {
    var component = renderComponent();
    component.refreshAttachments();
    var oldAttachments = component.state.attachments;

    component.onRemoveAttachment(oldAttachments[0]);

    var attachments = component.state.attachments;
    assert.notEqual(oldAttachments.length, attachments.length);
    assert.equal(oldAttachments[1], attachments[0]);
  });

  it("should render the modal", function () {
    var component = renderComponent();

    var modal = TestUtils.findRenderedComponentWithType(
      component, Modal.Container
    );

    assert.equal(modal.props.active, component.props.active);
  });

  it("should show the modal if it's visible", function () {
    var component = renderComponent({active: true});

    var modal = TestUtils.findRenderedComponentWithType(
      component, Modal.Container
    );

    assert.isTrue(modal.props.active);
  });

  it("should not render the attachments table if there's no attachments", function () {
    var component = renderComponent();

    assert.isUndefined(component.refs.signatoryAttachmentsTable);
  });

  it("should render the attachments table", function () {
    var component = renderComponent();
    component.refreshAttachments();

    var table = component.refs.signatoryAttachmentsTable;

    assert.equal(table.props.attachments, component.state.attachments);
    assert.equal(table.props.signatories, document_.signatories());
    assert.equal(table.props.onRemove, component.onRemoveAttachment);
  });

  it("should render the add attachment button", function () {
    var component = renderComponent();

    var button = component.refs.addAttachmentButton;
    assert.equal(button.props.onClick, component.onAddAttachmentButtonClick);
  });

  it("should add an attachment when add attachment button is clicked", function () {
    var component = renderComponent();
    var oldAttachments = component.state.attachments;

    var button = component.refs.addAttachmentButton;
    TestUtils.Simulate.click(component.refs.addAttachmentButton.getDOMNode());

    var attachments = component.state.attachments;
    assert.notEqual(oldAttachments.length, attachments.length);

    var newAttachment = attachments[attachments.length - 1];
    assert.equal(newAttachment.get("name"), "");
    assert.equal(newAttachment.get("description"), "");
    assert.isNull(newAttachment.get("signatory"));
    assert.isTrue(newAttachment.get("isRequired"));
  });

  it("should render the accept modal button", function () {
    var component = renderComponent();

    var button = component.refs.acceptModalButton;
    assert.equal(button.props.onClick, component.onAcceptButtonClick);
  });
});
