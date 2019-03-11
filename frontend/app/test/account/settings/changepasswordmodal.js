var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var backend = require("../../backend");
var util = require("../../util");

var ChangePasswordModal = require(
  "../../../scripts/account/settings/changepasswordmodal"
);
var PasswordService = require("../../../scripts/common/password_service");

var FlashMessage = require("../../../js/flashmessages.js");
var Submit = require("../../../js/submits.js");

describe("account/settings/changepasswordmodal", function () {
  var container = null;
  var fakeSubmit = null;

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  beforeEach(function () {
    fakeSubmit = new Submit.Submit();
    sinon.stub(fakeSubmit, "send");

    sinon.stub(FlashMessage, "FlashMessage");
    sinon.stub(Submit, "Submit").returns(fakeSubmit);
    sinon.stub(PasswordService, "checkPassword", function(_,callback) {callback();});

  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    FlashMessage.FlashMessage.restore();
    Submit.Submit.restore();
    PasswordService.checkPassword.restore();

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
        onClose: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(ChangePasswordModal, actualProps), container
    );

    return component;
  };

  it("should initialize state", function () {
    var component = renderComponent();
    assert.equal(component.state.oldPassword, "");
    assert.equal(component.state.newPassword, "");
    assert.equal(component.state.newPasswordAgain, "");
  });

  it("should clear state when it hides", function () {
    var component = renderComponent();
    component.setState({
      oldPassword: "spam",
      newPassword: "egs",
      newPasswordAgain: "spameggs"
    });

    component.onHide();
    assert.equal(component.state.oldPassword, "");
    assert.equal(component.state.newPassword, "");
    assert.equal(component.state.newPasswordAgain, "");
  });

  it("should hide when change password request succeeds", function () {
    var component = renderComponent();

    component.onSubmitSuccess({changed: true});
    assert.isTrue(component.props.onClose.called);
  });

  it("should display error message when change password request fails", function () {
    var component = renderComponent();

    component.onSubmitSuccess({changed: false});
    assert.isFalse(component.props.onClose.called);
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.validation.passwordOldPasswordNotValid
    }));
  });

  it("should configure and send change password request", function () {
    var component = renderComponent();
    component.setState({
      oldPassword: "spam",
      newPassword: "eggs",
      newPasswordAgain: "eggs"
    });

    component.savePassword();
    assert.isTrue(Submit.Submit.calledWithNew());
    assert.isTrue(Submit.Submit.calledWith({
      method: "POST",
      url: "/api/frontend/changepassword",
      oldpassword: "spam",
      password: "eggs",
      ajax: true,
      ajaxsuccess: component.onSubmitSuccess
    }));
    assert.isTrue(fakeSubmit.send.called);
  });

  it("should display error message if new password is too short", function () {
    var component = renderComponent();
    sinon.stub(component, "savePassword");

    component.setState({
      oldPassword: "spam",
      newPassword: "eggs",
      newPasswordAgain: "eggs"
    });

    component.onAcceptButtonClick();
    assert.isFalse(component.savePassword.called);
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.validation.passwordLessThanMinLength
    }));
  });

  it("should display error message if new password is too long", function () {
    var password = underscore.map(underscore.range(251), function () {
      return "a";
    });

    var component = renderComponent();
    sinon.stub(component, "savePassword");

    component.setState({
      oldPassword: "spam",
      newPassword: password.join(""),
      newPasswordAgain: "eggs"
    });

    component.onAcceptButtonClick();
    assert.isFalse(component.savePassword.called);
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.validation.passwordExceedsMaxLength
    }));
  });

  it("should display error message if new password doesn't contain at least one letter and digit", function () {
    var component = renderComponent();
    sinon.stub(component, "savePassword");

    component.setState({
      oldPassword: "_-_-_-_-",
      newPassword: "eggs",
      newPasswordAgain: "eggs"
    });

    component.onAcceptButtonClick();
    assert.isFalse(component.savePassword.called);
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
  });

  it("should display error message if new passwords aren't equal", function () {
    var component = renderComponent();
    sinon.stub(component, "savePassword");

    component.setState({
      oldPassword: "spam",
      newPassword: "eggs123412341234",
      newPasswordAgain: "eggs12341234"
    });

    component.onAcceptButtonClick();
    assert.isFalse(component.savePassword.called);
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.validation.passwordsDontMatch
    }));
  });

  it("should save new password", function () {
    var component = renderComponent();
    sinon.stub(component, "savePassword");

    component.setState({
      oldPassword: "spam",
      newPassword: "eggs12341234",
      newPasswordAgain: "eggs12341234"
    });

    component.onAcceptButtonClick();
    assert.isTrue(component.savePassword.called);
  });

  it("should update state when old password field changes", function () {
    var component = renderComponent();

    component.onOldPasswordChange({target: {value: "spam"}});
    assert.equal(component.state.oldPassword, "spam");
  });

  it("should update state when new password field changes", function () {
    var component = renderComponent();

    component.onNewPasswordChange({target: {value: "spam"}});
    assert.equal(component.state.newPassword, "spam");
  });

  it("should update state when repeat new password field changes", function () {
    var component = renderComponent();

    component.onNewPasswordAgainChange({target: {value: "spam"}});
    assert.equal(component.state.newPasswordAgain, "spam");
  });

  it("should configure and render the old password field", function () {
    var component = renderComponent();
    component.setState({oldPassword: "spam"});

    assert.isDefined(component.refs.oldPassword);
    assert.equal(
      component.refs.oldPassword.props.value, component.state.oldPassword
    );
    assert.equal(
      component.refs.oldPassword.props.onChange, component.onOldPasswordChange
    );
  });

  it("should configure and render the new password field", function () {
    var component = renderComponent();
    component.setState({newPassword: "spam"});

    assert.isDefined(component.refs.newPassword);
    assert.equal(
      component.refs.newPassword.props.value, component.state.newPassword
    );
    assert.equal(
      component.refs.newPassword.props.onChange, component.onNewPasswordChange
    );
  });

  it("should configure and render the repeat new password field", function () {
    var component = renderComponent();
    component.setState({newPasswordAgain: "spam"});

    assert.isDefined(component.refs.newPasswordAgain);
    assert.equal(
      component.refs.newPasswordAgain.props.value,
      component.state.newPasswordAgain
    );
    assert.equal(
      component.refs.newPasswordAgain.props.onChange,
      component.onNewPasswordAgainChange
    );
  });
});
