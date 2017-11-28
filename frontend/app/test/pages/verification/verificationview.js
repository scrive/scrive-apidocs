var React = require("react");
var underscore = require("underscore");

var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var LoadingDialog = require("../../../js/loading.js");
var ResultModal = require(
  "../../../scripts/pages/verification/resultmodal.jsx"
);
var Submits = require("../../../js/submits.js");
var UploadButton = require("../../../scripts/common/uploadbutton.jsx");
var VerificationView = require("../../../scripts/pages/verification");

describe("pages/verification", function () {
  var container = null;
  var fakeSubmit = null;

  beforeEach(function () {
    fakeSubmit = new Submits.Submit();
    sinon.stub(fakeSubmit, "addInputs");
    sinon.stub(fakeSubmit, "send");

    sinon.stub(LoadingDialog.LoadingDialog, "close");
    sinon.stub(LoadingDialog.LoadingDialog, "open");
    sinon.stub(Submits, "Submit").returns(fakeSubmit);
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    LoadingDialog.LoadingDialog.close.restore();
    LoadingDialog.LoadingDialog.open.restore();
    Submits.Submit.restore();

    util.cleanTimeoutsAndBody();
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {},
      props || {}
    );

    var component = React.render(
      React.createElement(VerificationView, actualProps), container
    );

    return component;
  };

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.showResultModal);
    assert.isNull(component.state.verificationResult);
    assert.isNull(component.state.verificationTime);
  });

  it("should reset state to default when result modal closes", function () {
    var component = renderComponent();
    component.setState({
      showResultModal: true,
      verificationResult: "success",
      verificationTime: (new Date()).toISOString()
    });

    component.onResultModalClose();
    assert.isFalse(component.state.showResultModal);
    assert.isNull(component.state.verificationResult);
    assert.isNull(component.state.verificationTime);
  });

  it("should close the loading dialog when verification requests fails", function () {
    var component = renderComponent();

    component.onSubmitError();
    assert.isTrue(LoadingDialog.LoadingDialog.close.called);
  });

  it("should open the loading dialog when verification requests is sent", function () {
    var component = renderComponent();

    component.onSubmitSend();
    assert.isTrue(LoadingDialog.LoadingDialog.open.called);
  });

  describe("onSubmitSuccess", function () {
    it("should close the loading dialog", function () {
      var component = renderComponent();

      component.onSubmitSuccess({error: true});
      assert.isTrue(LoadingDialog.LoadingDialog.close.called);
    });

    it("should update state to show the result modal", function () {
      var component = renderComponent();

      component.onSubmitSuccess({error: true});
      assert.isTrue(component.state.showResultModal);
    });

    it("should update state for successful verification", function () {
      var time = (new Date()).toISOString();
      var component = renderComponent();

      component.onSubmitSuccess({success: true, time: time});
      assert.equal(component.state.verificationResult, "success");
      assert.equal(component.state.verificationTime, time);
    });

    it("should update state verification error", function () {
      var component = renderComponent();

      component.onSubmitSuccess({error: true});
      assert.equal(component.state.verificationResult, "error");
      assert.isNull(component.state.verificationTime);
    });

    it("should update state verification failure", function () {
      var component = renderComponent();

      component.onSubmitSuccess({});
      assert.equal(component.state.verificationResult, "failed");
      assert.isNull(component.state.verificationTime);
    });
  });

  it("should configure and send verification request", function () {
    var component = renderComponent();

    component.onUploadComplete("spam");
    assert.isTrue(Submits.Submit.calledWithNew());
    assert.isTrue(Submits.Submit.calledWith({
      method: "POST",
      ajax: true,
      url: "/verify",
      onSend: component.onSubmitSend,
      ajaxerror: component.onSubmitError,
      ajaxsuccess: component.onSubmitSuccess
    }));
    assert.isTrue(fakeSubmit.addInputs.calledWith("spam"));
    assert.isTrue(fakeSubmit.send.called);
  });

  describe("render", function () {
    it("should configure and render the upload button", function () {
      var component = renderComponent();

      var button = TestUtils.findRenderedComponentWithType(
        component, UploadButton
      );
      assert.equal(button.props.name, "file");
      assert.equal(button.props.size, "big");
      assert.isTrue(button.props.submitOnUpload);
      assert.equal(button.props.text, localization.uploadButton);
      assert.equal(button.props.type, "action");
      assert.equal(button.props.width, 380);
      assert.equal(button.props.onUploadComplete, component.onUploadComplete);
    });

    it("should configure and render the result modal", function () {
      var component = renderComponent();
      component.setState({
        showResultModal: true,
        verificationResult: "success",
        verificationTime: (new Date()).toISOString()
      });

      var modal = TestUtils.findRenderedComponentWithType(
        component, ResultModal
      );
      assert.equal(modal.props.active, component.state.showResultModal);
      assert.equal(modal.props.result, component.state.verificationResult);
      assert.equal(modal.props.time, component.state.verificationTime);
      assert.equal(modal.props.onClose, component.onResultModalClose);
    });
  });
});
