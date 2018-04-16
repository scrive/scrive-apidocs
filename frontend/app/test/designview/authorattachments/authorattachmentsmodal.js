var jQuery = require("jquery");
var React = require("react");
var underscore = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var AuthorAttachmentsModal = require(
  "../../../scripts/designview/authorattachments/authorattachmentsmodal"
);
var DesignViewAttachment = require(
  "../../../scripts/designview/authorattachments/designviewattachment"
);
var DesignViewAttachments = require(
  "../../../scripts/designview/authorattachments/designviewattachments"
);
var FlashMessage = require("../../../js/flashmessages.js");
var Submit = require("../../../js/submits.js").Submit;

describe("designview/authorattachments/authorattachmentsmodal", function () {
  var server, document_;
  var container = null;

  var renderComponent = function(props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        active: false,
        document: document_,
        saveAndFlashMessageIfAlreadySaved: sinon.stub(),
        onClose: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(AuthorAttachmentsModal, actualProps), container
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      sinon.stub(FlashMessage, "FlashMessage");
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

    FlashMessage.FlashMessage.restore();

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isTrue(component.state.acceptVisible);
    assert.isTrue(component.state.cancelVisible);
    assert.isFalse(component.state.extraButtonsVisible);
  });

  describe("componentWillMount", function () {
    it("should initialize the model if it's active", function () {
      var component = renderComponent();

      component.componentWillReceiveProps({active: true});
      assert.isDefined(component._model);
      assert.instanceOf(component._model, DesignViewAttachments);
    });

    it("should reset state to default when it becomes active", function () {
      var component = renderComponent();
      component.setState({
        acceptVisible: false,
        cancelVisible: false,
        extraButtonsVisible: true
      });

      component.componentWillReceiveProps({active: true});
      assert.isTrue(component.state.acceptVisible);
      assert.isTrue(component.state.cancelVisible);
      assert.isFalse(component.state.extraButtonsVisible);
    });
  });

  describe("attachmentsToSave", function () {
    var model = null;
    var submit = null;

    beforeEach(function () {
      model = new DesignViewAttachments({document: document_});
    });

    it("should configure an attachment chosen from list", function () {
      var component = renderComponent();
      component._model = model;

      var attachment = new DesignViewAttachment({
        name: "spam",
        required: true,
        addToSealedFile: true,
        serverFileId: "spam",
        fileUpload: undefined,
        documentid: document_.documentid()
      });
      model.addAttachment(attachment);

      var result = component.attachmentsToSave();
      assert.equal(result[0].file_id, attachment.serverFileId());
      assert.isUndefined(result[0].file_param);
    });

    it("should configure attachments metadata", function () {
      var component = renderComponent();
      component._model = model;

      var spamAttachment = new DesignViewAttachment({
        name: "spam",
        required: true,
        addToSealedFile: true,
        serverFileId: "spam",
        fileUpload: undefined,
        documentid: document_.documentid()
      });
      model.addAttachment(spamAttachment);

      var eggsAttachment = new DesignViewAttachment({
        name: "eggs",
        required: false,
        addToSealedFile: false,
        serverFileId: "eggs",
        fileUpload: undefined,
        documentid: document_.documentid()
      });
      model.addAttachment(eggsAttachment);

      var result = component.attachmentsToSave(submit);
      assert.lengthOf(result, 2);

      assert.equal(result[0].name, spamAttachment.name());
      assert.equal(result[0].required, spamAttachment.isRequired());
      assert.equal(
        result[0].add_to_sealed_file, spamAttachment.isAddToSealedFile()
      );
    });
  });

  it("should save already-uploaded attachments", function () {
    var submit = new Submit();
    sinon.stub(submit, "add");
    sinon.stub(submit, "sendAjax", function(callback, _) {
      callback();
    });
    sinon.stub(document_, "setAttachments").returns(submit);

    var component = renderComponent();
    sinon.stub(component, "attachmentsToSave").returns("spam");
    sinon.stub(component, "attachmentsToUpload").returns([]);

    component.saveServerAttachments(function() { });
    assert.isTrue(submit.add.calledWith("attachments", '"spam"'));
    assert.isTrue(submit.sendAjax.called);
    assert.isFalse(component.isLoading());
  });

  it("should upload new attachments", function () {
    var submit = new Submit();
    sinon.stub(submit, "add");
    sinon.stub(submit, "sendAjax", function (callback, _) {
      callback();
    });
    sinon.stub(document_, "setAttachments").returns(submit);

    var file = {};
    var req = {
      add: function() {},
      sendAjax: function(success, error) {
        success({
          responseText: "{\"author_attachments\":[{\"name\":\"My File\",\"file_id\":1337}]}"
        });
      }
    };
    sinon.stub(req, "add");
    sinon.stub(document_, "setAttachmentsIncrementally").returns(req);

    var component = renderComponent();
    sinon.stub(component, "attachmentsToSave").returns("spam");

    var attachment = new DesignViewAttachment({
      name: "My File",
      fileUpload: file
    });
    sinon.stub(component, "attachmentsToUpload").returns([attachment]);

    component.uploadNewAttachments(function() { });
    assert.isTrue(attachment.serverFileId() == 1337);
    assert.isTrue(attachment.fileUpload() === undefined);
    assert.isTrue(req.add.calledWith("file", file));
    assert.isTrue(req.add.calledWith("attachments"));
    assert.isFalse(component.isLoading());
  });

  it("should show an error when an attachment is too large", function () {
    var file = sinon.mock({});
    var attachment = new DesignViewAttachment({
      name: "My File",
      fileUpload: file
    });
    var req = {
      add: function () {},
      sendAjax: function(success, error) {
        error({status: 413});
      }
    };
    sinon.stub(document_, "setAttachmentsIncrementally").returns(req);

    var component = renderComponent();
    sinon.stub(component, "attachmentsToUpload").returns([attachment]);

    component.uploadNewAttachments();
    assert.isFalse(component.isLoading());
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.authorattachments.tooLargeAttachment + " (My File)"
    }));
  });

  it("should show an error when an attachment is invalid", function () {
    var file = sinon.mock({});
    var attachment = new DesignViewAttachment({
      name: "My File",
      fileUpload: file
    });
    var req = {
      add: function () {},
      sendAjax: function(success, error) {
        error({status: 400});
      }
    };
    sinon.stub(document_, "setAttachmentsIncrementally").returns(req);

    var component = renderComponent();
    sinon.stub(component, "attachmentsToUpload").returns([attachment]);

    component.uploadNewAttachments();
    assert.isFalse(component.isLoading());
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.authorattachments.invalidAttachments + " (My File)"
    }));
  });

  it("should reset the model when it hides", function () {
    var component = renderComponent();
    component._model = new DesignViewAttachments({
      document: document_
    });

    component.onHide();
    assert.isNull(component._model);
  });

  it("should handle the document being recalled", function () {
    sinon.stub(document_, "trigger");
    var component = renderComponent();
    component._model = {
      hasErrorMessages: function () {
        return false;
      }
    };

    component.onRecallDocument();
    assert.isTrue(document_.trigger.calledWith("change"));
    assert.isTrue(component.props.saveAndFlashMessageIfAlreadySaved.called);
    assert.isTrue(component.props.onClose.called);
  });

  describe("onSaveAttachmentsError", function () {
    var component = null;

    beforeEach(function () {
      component = renderComponent();
      component._model = new DesignViewAttachments({
        document: document_
      });
    });

    it("should display a generic error message", function () {
      var attachment = new DesignViewAttachment({
        name: "Attachment"
      });
      sinon.stub(component._model, "attachments").returns([attachment]);

      component.onSaveAttachmentsError({status: 400});
      assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
      assert.isTrue(FlashMessage.FlashMessage.calledWith({
        type: "error",
        content: localization.authorattachments.invalidAttachments
      }));
    });

    it("should close the loading dialog", function () {
      component.onSaveAttachmentsError({status: 400});
      assert.isFalse(component.isLoading());
    });
  });

  it("should update state when attachments list is shown", function () {
    var component = renderComponent();

    component.onStartShowingList();
    assert.isFalse(component.state.acceptVisible);
    assert.isFalse(component.state.cancelVisible);
    assert.isTrue(component.state.extraButtonsVisible);
  });

  it("should update state when attachments list is hidden", function () {
    var component = renderComponent();
    component.setState({
      acceptVisible: false,
      cancelVisible: false,
      extraButtonsVisible: true
    });

    component.onStopShowingList();
    assert.isTrue(component.state.acceptVisible);
    assert.isTrue(component.state.cancelVisible);
    assert.isFalse(component.state.extraButtonsVisible);
  });

  describe("onAcceptButtonClick", function () {
    var component = null;

    beforeEach(function () {
      sinon.stub(document_, "afterSave");

      component = renderComponent();
      component._model = new DesignViewAttachments({
        document: document_
      });

      component._model.addAttachment(new DesignViewAttachment({
        name: "spam",
        required: true,
        addToSealedFile: true,
        serverFileId: "spam",
        fileUpload: undefined,
        documentid: document_.documentid()
      }));
    });

    it("should display an error message if not all attachments are uniquely named", function () {
      component._model.addAttachment(new DesignViewAttachment({
        name: "spam",
        required: true,
        addToSealedFile: true,
        serverFileId: "spam",
        fileUpload: undefined,
        documentid: document_.documentid()
      }));

      var result = component.onAcceptButtonClick();
      assert.isFalse(result);

      assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
      assert.isTrue(FlashMessage.FlashMessage.calledWith({
        type: "error",
        content: localization.signatoryAttachments.uniqueAttachmentNamesError
      }));
      assert.isFalse(document_.afterSave.called);
    });

    it("should save attachments", function () {
      var result = component.onAcceptButtonClick();
      assert.isTrue(result);
      assert.isFalse(FlashMessage.FlashMessage.called);
      assert.isTrue(document_.afterSave.calledWith(component.saveAttachments));
    });
  });

  it("should hide attachments list when back button is clicked", function () {
    var component = renderComponent({active: true});
    sinon.stub(component.refs.contentView, "stopShowingAttachmentList");

    var fakeEvent = {
      preventDefault: sinon.stub(),
      stopPropagation: sinon.stub()
    };

    component.onBackButtonClick(fakeEvent);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(component.refs.contentView.stopShowingAttachmentList.called);
  });

  describe("render", function () {
    it("should not render the content view if it mounts inactive", function () {
      var component = renderComponent();
      assert.isUndefined(component.refs.contentView);
    });

    it("should configure and render the content view", function () {
      var component = renderComponent({active: true});
      assert.isDefined(component.refs.contentView);
      assert.equal(component.refs.contentView.props.model, component._model);
      assert.equal(
        component.refs.contentView.props.onStartShowingList,
        component.onStartShowingList
      );
      assert.equal(
        component.refs.contentView.props.onStopShowingList,
        component.onStopShowingList
      );
    });

    it("should not render the cancel button if it isn't visible", function () {
      var component = renderComponent();
      component.setState({cancelVisible: false});

      assert.isUndefined(component.refs.cancelButton);
    });

    it("should configure and render the cancel button", function () {
      var component = renderComponent();
      component.setState({cancelVisible: true});

      assert.isDefined(component.refs.cancelButton);
      assert.equal(
        component.refs.cancelButton.props.onClick, component.props.onClose
      );
    });

    it("should not render the back button if extra buttons aren't visible", function () {
      var component = renderComponent();
      component.setState({extraButtonsVisible: false});

      assert.isUndefined(component.refs.backButton);
    });

    it("should configure and render the back button", function () {
      var component = renderComponent();
      component.setState({extraButtonsVisible: true});

      assert.isDefined(component.refs.backButton);
      assert.equal(
        component.refs.backButton.props.onClick, component.onBackButtonClick
      );
    });

    it("should not render the accept button if it isn't visible", function () {
      var component = renderComponent();
      component.setState({acceptVisible: false});

      assert.isUndefined(component.refs.acceptButton);
    });

    it("should configure and render the accept button", function () {
      var component = renderComponent();
      component.setState({acceptVisible: true});

      assert.isDefined(component.refs.acceptButton);
      assert.equal(
        component.refs.acceptButton.props.onClick,
        component.onAcceptButtonClick
      );
    });
  });
});
