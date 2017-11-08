var React = require("react");
var underscore = require("underscore");

var backend = require("../../../backend");
var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var BodyView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/bodyview.jsx"
);
var CSVSignatoryDesignModal = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal"
);
var Signatory = require("../../../../js/signatories.js").Signatory;
var UploadButton = require("../../../../scripts/common/uploadbutton");

describe("designview/participants/csvsignatorydesignmodal", function () {
  var container = null;
  var document_ = null;

  before(function () {
    server = backend.createServer();
  })

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      done();
    });
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    server.restore();
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        active: false,
        document: document_,
        setParticipantDetail: sinon.stub(),
        onClose: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(CSVSignatoryDesignModal, actualProps), container
    );

    return component;
  };

  it("should create the model when it becomes active", function () {
    var component = renderComponent({active: false});
    sinon.spy(component, "createModel");

    component.componentWillReceiveProps({active: true});
    assert.isTrue(component.createModel.called);
  });

  it("should destroy the model with it becomes inactive", function () {
    var component = renderComponent({active: false});
    sinon.spy(component, "destroyModel");

    component.componentDidUpdate({active: true});
    assert.isTrue(component.destroyModel.called);
  });

  it("should destroy the model when it unmounts", function () {
    var component = renderComponent();
    sinon.spy(component, "destroyModel");

    React.unmountComponentAtNode(container);
    container = null;

    assert.isTrue(component.destroyModel.called);
  });

  describe("createModel", function () {
    it("should destroy the old model before creating a new one", function () {
      var component = renderComponent();
      sinon.stub(component, "csvSignatory").returns(undefined);
      sinon.stub(component, "destroyModel");

      component.createModel();
      assert.isTrue(component.destroyModel.called);
    });

    it("should create empty model if there's no CSV signatory", function () {
      var component = renderComponent();
      sinon.stub(component, "csvSignatory").returns(undefined);

      component.createModel();
      assert.isNotNull(component._model);
      assert.isUndefined(component._model.header());
      assert.lengthOf(component._model.rows(), 0);
    });

    it("should use CSV signatory to initialize the model", function () {
      var csv = [
        ["fstname", "sndname", "email"],
        ["Spam", "Eggs", "spam@eggs.com"],
        ["Eggs", "Spam", "eggs@spam.com"]
      ];

      var csvSignatory = document_.signatoriesWhoSign()[0];
      csvSignatory.setCsv(csv);

      var component = renderComponent();
      sinon.stub(component, "csvSignatory").returns(csvSignatory);

      component.createModel();
      assert.isNotNull(component._model);
      assert.equal(component._model.header(), csv[0])
      assert.lengthOf(component._model.rows(), 2);
      assert.equal(component._model.rows()[0], csv[1])
    });
  });

  it("should destroy the model", function () {
    var component = renderComponent();
    sinon.stub(component, "csvSignatory").returns(undefined);

    component.createModel();
    var model = component._model;
    sinon.spy(model, "off");

    component.destroyModel();
    assert.isNull(component._model);
    assert.isTrue(model.off.calledWith("change", component.onModelChange));
  });

  describe("csvSignatory", function () {
    it("should return undefined if the document has no CSV signatories", function () {
      var signatories = document_.signatories();
      for(var i = 0; i < signatories.length; i++) {
        sinon.stub(signatories[i], "isCsv").returns(false);
      }

      var component = renderComponent();

      var result = component.csvSignatory();
      assert.isUndefined(result);
    });

    it("should return the document's last CSV signatory", function () {
      var signatories = document_.signatories();
      for(var i = 0; i < signatories.length; i++) {
        sinon.stub(signatories[i], "isCsv").returns(true);
      }

      var component = renderComponent();

      var result = component.csvSignatory();
      assert.equal(result, signatories[signatories.length - 1]);
    });
  });

  describe("modalWidth", function () {
    it("should return width without data if model is null", function () {
      var component = renderComponent();
      component._model = null;

      assert.equal(component.modalWidth(), component.WIDTH_WITHOUT_DATA);
    });

    it("should return width without data if the model has no data", function () {
      var component = renderComponent();

      component.createModel();
      sinon.stub(component._model, "hasData").returns(false);

      assert.equal(component.modalWidth(), component.WIDTH_WITHOUT_DATA);
    });

    it("should return width with data if the model has data", function () {
      var component = renderComponent();

      component.createModel();
      sinon.stub(component._model, "hasData").returns(true);

      assert.equal(component.modalWidth(), component.WIDTH_WITH_DATA);
    });
  });

  it("should convert header to fields", function () {
    var component = renderComponent();
    component.createModel();
    component._model.set({header: ["fstname", "spam"]});

    var result = component.headerToFields();
    assert.lengthOf(result, 2);
    assert.equal(result[0], component._model.csvstandardheaders["fstname"]);

    var customField = result[1];
    assert.equal(customField.type, "text");
    assert.equal(customField.name, "spam");
  });

  describe("onAcceptButtonClick", function () {
    it("should create a CSV signatory if the document doesn't have one", function () {
      sinon.stub(document_, "addExistingSignatory");

      var component = renderComponent();
      sinon.stub(component, "csvSignatory").returns(undefined);

      component.createModel();

      component._model.set({
        header: ["fstname", "spam"],
        rows: [
          ["Spam", "spam"],
          ["Eggs", "eggs"]
        ]
      });

      component.onAcceptButtonClick();
      assert.isTrue(document_.addExistingSignatory.called);

      var signatory = document_.addExistingSignatory.args[0][0];
      assert.equal(signatory.document(), document_);
      assert.isTrue(signatory.get("signs"));

      var signatoryCSV = signatory.csv();
      assert.equal(signatoryCSV[0], component._model.header());
      assert.equal(signatoryCSV[1], component._model.rows()[0]);
      assert.equal(signatoryCSV[2], component._model.rows()[1]);

      assert.isTrue(
        component.props.setParticipantDetail.calledWith(signatory)
      );
    });

    it("should update the document's CSV signatory", function () {
      sinon.stub(document_, "addExistingSignatory");

      var signatory = document_.signatoriesWhoSign()[0];
      signatory.setCsv([]);
      sinon.stub(signatory, "addField");

      var component = renderComponent();
      sinon.stub(component, "csvSignatory").returns(signatory);

      component.createModel();

      component._model.set({
        header: ["fstname", "spam"],
        rows: [
          ["Spam", "spam"],
          ["Eggs", "eggs"]
        ]
      });

      component.onAcceptButtonClick();
      assert.isFalse(document_.addExistingSignatory.called);

      assert.equal(signatory.addField.callCount, 2);

      var addedNameField = signatory.addField.firstCall.args[0];
      assert.equal(addedNameField.signatory(), signatory);
      assert.equal(addedNameField.type(), "name");

      var addedCustomField = signatory.addField.secondCall.args[0];
      assert.equal(addedCustomField.signatory(), signatory);
      assert.equal(addedCustomField.type(), "text");
      assert.equal(addedCustomField.name(), "spam");

      var signatoryCSV = signatory.csv();
      assert.equal(signatoryCSV[0], component._model.header());
      assert.equal(signatoryCSV[1], component._model.rows()[0]);
      assert.equal(signatoryCSV[2], component._model.rows()[1]);
    });

    it("should close the modal", function () {
      var component = renderComponent();
      component.createModel();

      component.onAcceptButtonClick();
      assert.isTrue(component.props.onClose.called);
    });
  });

  it("should pass the uploaded file to the model for processing", function () {
    var component = renderComponent();

    component.createModel();
    sinon.stub(component._model, "upload");

    component.onCSVUploadComplete("spam");
    assert.isTrue(component._model.upload.calledWith("spam"));
  });

  describe("render", function () {
    it("should not render the conent view if there's no model", function () {
      var component = renderComponent();
      assert.isUndefined(component.refs.contentView);
    });

    it("should render the subtitle if the model has no rows", function () {
      var component = renderComponent();
      component.createModel();
      component.forceUpdate();

      var subtitleView = TestUtils.findRenderedDOMComponentWithClass(
        component.refs.contentView, "modal-subtitle"
      );
      assert.equal(
        subtitleView.getDOMNode().innerText, localization.csv.subtitle
      );
    });

    it("should render the subtitle if the model has rows", function () {
      var component = renderComponent();

      component.createModel();
      component._model.set({rows: [["spam", "eggs"]]}, {silent: true});
      component.forceUpdate();

      var subtitleView = TestUtils.findRenderedDOMComponentWithClass(
        component.refs.contentView, "modal-subtitle"
      );
      assert.equal(
        subtitleView.getDOMNode().innerText,
        "1 " + localization.designview.participantsInFile
      );
    });

    it("should not render the body view if the model is empty", function () {
      var component = renderComponent();

      component.createModel();
      sinon.stub(component._model, "isEmpty").returns(true);
      component.forceUpdate();

      var bodyViews = TestUtils.scryRenderedComponentsWithType(
        component.refs.contentView, BodyView
      );
      assert.lengthOf(bodyViews, 0);
    });

    it("should configure and render the body view if the model isn't empty", function () {
      var component = renderComponent();

      component.createModel();
      sinon.stub(component._model, "isEmpty").returns(false);
      component.forceUpdate();

      var bodyView = TestUtils.findRenderedComponentWithType(
        component.refs.contentView, BodyView
      );
      assert.equal(bodyView.props.model, component._model);
    });

    it("should configure and render the upload button", function () {
      var component = renderComponent();

      component.createModel();
      component.forceUpdate();

      var uploadButton = TestUtils.findRenderedComponentWithType(
        component.refs.contentView, UploadButton
      );
      assert.equal(
        uploadButton.props.onUploadComplete, component.onCSVUploadComplete
      );
    });

    it("should not render the accept button if there's no model", function () {
      var component = renderComponent();
      assert.isUndefined(component.refs.acceptModalButton);
    });

    it("should not render the accept button if the model isn't ready", function () {
      var component = renderComponent();

      component.createModel();
      sinon.stub(component._model, "ready").returns(false);
      component.forceUpdate();

      assert.isUndefined(component.refs.acceptModalButton);
    });

    it("should configure and render the accept button", function () {
      var component = renderComponent();

      component.createModel();
      sinon.stub(component._model, "ready").returns(true);
      component.forceUpdate();

      assert.isDefined(component.refs.acceptModalButton);
      assert.equal(
        component.refs.acceptModalButton.props.onClick,
        component.onAcceptButtonClick
      );
    })
  });
});
