var jQuery = require("jquery");
var React = require("react");
var underscore = require("underscore");

var util = require("../util");

var TestUtils = React.addons.TestUtils;

var NewPasswordView = require("../../scripts/account/newpassword");
var FlashMessages = require("../../js/flashmessages.js");
var Submits = require("../../js/submits.js");
var Validation = require("../../js/validation.js");
var PasswordService = require("../../scripts/common/password_service");


describe("account/newpassword", function () {
  var container = null;
  var fakeSubmit = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn(
      {url: "/amnesia/1/spam"}, props || {}
    );

    var component = React.render(
      React.createElement(NewPasswordView, actualProps), container
    );

    return component;
  };

  beforeEach(function () {
    fakeSubmit = new Submits.Submit();
    sinon.stub(fakeSubmit, "send");

    sinon.stub(FlashMessages, "FlashMessage");
    sinon.stub(Submits, "Submit").returns(fakeSubmit);
    sinon.stub(PasswordService, "checkPassword", function(_,callback) {callback();});

  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    FlashMessages.FlashMessage.restore();
    Submits.Submit.restore();
    PasswordService.checkPassword.restore();


    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.equal(component.state.password, "");
    assert.equal(component.state.repeatPassword, "");
  });

  describe("componentWillMount", function () {
    it("should initialize password validation error", function () {
      var component = renderComponent();
      assert.isNull(component._passwordValidationError);
    });

    it("should create password validator", function () {
      var component = renderComponent();
      assert.instanceOf(
        component._passwordValidation, Validation.PasswordValidation
      );
      assert.equal(
        component._passwordValidation.get("callback"),
        component.onPasswordValidationFail
      );
      assert.equal(
        component._passwordValidation.get("message"),
        localization.validation.passwordLessThanMinLength
      );
      assert.equal(
        component._passwordValidation.get("message_max"),
        localization.validation.passwordExceedsMaxLength
      );
    });
  });

  describe("validateAll", function () {
    it("should return false is passwords don't match", function () {
      var component = renderComponent();
      component.setState({
        password: "spam",
        repeatPassword: "eggs"
      });

      var result = component.validateAll();
      assert.isFalse(result);
      assert.equal(
        component._passwordValidationError,
        localization.newPasswordModal.flashMessagePasswordsDontMatch
      );
    });

    it("should validate the password and return the result", function () {
      var component = renderComponent();
      sinon.stub(component._passwordValidation, "validateData").returns(false);

      var result = component.validateAll();
      assert.isFalse(result);
      assert(component._passwordValidation.validateData.calledWith(
        component.state.password
      ));
    });
  });

  it("should set password error mesage when validation fails", function () {
    var component = renderComponent();

    component.onPasswordValidationFail(
      "spam123", null, component._passwordValidation
    );
    assert.equal(
      component._passwordValidationError,
      component._passwordValidation.message()
    );
  });

  describe("onSubmitSuccess", function () {
    it("should handle error", function () {
      var component = renderComponent();

      component.onSubmitSuccess({logged: false});
      assert.isTrue(FlashMessages.FlashMessage.calledWithNew());
      assert.isTrue(FlashMessages.FlashMessage.calledWith({
        content: localization.newPasswordModal.flashMessagePasswordChangeLinkNotValid,
        type: "error"
      }));
    });
  });

  it("should save when Enter key is pressed on an input", function () {
    var component = renderComponent();
    sinon.stub(component, "onSaveButtonClick");

    component.onInputEnter();
    assert.isTrue(component.onSaveButtonClick.called);
  });

  it("should update state when the password input changes", function () {
    var component = renderComponent();

    component.onPasswordInputChange("spam");
    assert.equal(component.state.password, "spam");
  });

  it("should update state when the repeat password input changes", function () {
    var component = renderComponent();

    component.onRepeatPasswordInputChange("spam");
    assert.equal(component.state.repeatPassword, "spam");
  });

  describe("onSaveButtonClick", function () {
    it("should display error message if the validation fails", function () {
      var component = renderComponent();
      component._passwordValidationError = "spam";
      sinon.stub(component, "validateAll").returns(false);

      component.onSaveButtonClick();
      assert.isTrue(FlashMessages.FlashMessage.calledWithNew());
      assert.isTrue(FlashMessages.FlashMessage.calledWith({
        content: component._passwordValidationError,
        type: "error"
      }));
      assert.isFalse(Submits.Submit.calledWithNew());
    });

    it("should configure and send save password request", function () {
      var component = renderComponent();
      component.setState({
        password: "password1234",
        repeatPassword: "password1234"
      });

      component.onSaveButtonClick();
      assert.isTrue(Submits.Submit.calledWithNew());
      assert.isTrue(Submits.Submit.calledWith({
        url: component.props.url,
        method: "POST",
        ajax: true,
        password: "password1234",
        ajaxsuccess: component.onSubmitSuccess
      }));
      assert.isTrue(fakeSubmit.send.called);
    });
  });

  describe("render", function () {
    it("should configure and render the password input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.passwordInput);
      assert.equal(component.refs.passwordInput.props.inputtype, "password");
      assert.equal(
        component.refs.passwordInput.props.value, component.state.password
      );
      assert.equal(
        component.refs.passwordInput.props.onChange,
        component.onPasswordInputChange
      );
    });

    it("should configure and render the repeat password input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.repeatPasswordInput);
      assert.equal(
        component.refs.repeatPasswordInput.props.inputtype, "password"
      );
      assert.equal(
        component.refs.repeatPasswordInput.props.value,
        component.state.repeatPassword
      );
      assert.equal(
        component.refs.repeatPasswordInput.props.onChange,
        component.onRepeatPasswordInputChange
      );
    });

    it("should configure and render the save button", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.saveButton);
      assert.equal(
        component.refs.saveButton.props.onClick,
        component.onSaveButtonClick
      );
    });
  });
});
