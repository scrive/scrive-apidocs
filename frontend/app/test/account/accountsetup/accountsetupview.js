var jQuery = require("jquery");
var React = require("react");
var underscore = require("underscore");

var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var AccountSetupView = require(
  "../../../scripts/account/setup/accountsetupview"
);
var Submits = require("../../../js/submits.js");
var Validation = require("../../../js/validation.js");
var PasswordService = require("../../../scripts/common/password_service");

describe("account/setup/accountsetupview", function () {
  var container = null;

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }
    PasswordService.checkPassword.restore();
    util.cleanTimeoutsAndBody();
  });

  beforeEach(function () {
    sinon.stub(PasswordService, "checkPassword", function(_,callback) {callback();});
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        fstname: "Spam",
        sndname: "Eggs",
        email: "spam@eggs.com",
        company: "Spameggs",
        userid: "1",
        signupmethod: "CompanyInvitation",
        position: "CEO",
        phone: "+48123456789",
        companyAdmin: false,
        invitationId: "eggsspam"
      },
      props || {}
    );

    var component = React.render(
      React.createElement(AccountSetupView, actualProps), container
    );

    return component;
  };

  it("should initialize state", function () {
    var component = renderComponent();
    assert.equal(component.state.fullName, component.fullName());
    assert.isTrue(component.state.fullNameValid);
    assert.equal(component.state.company, component.props.company);
    assert.equal(component.state.position, component.props.position);
    assert.equal(component.state.phone, component.props.phone);
    assert.equal(component.state.password, "");
    assert.isTrue(component.state.passwordValid);
    assert.equal(component.state.passwordError, "");
    assert.equal(component.state.repeatPassword, "");
    assert.isTrue(component.state.repeatPasswordValid);
    assert.isFalse(component.state.acceptTOS);
    assert.isTrue(component.state.acceptTOSValid);
  });

  describe("componentWillMount", function () {
    it("should create full name validator", function () {
      var component = renderComponent();
      assert.instanceOf(
        component._fullNameValidation, Validation.NameValidation
      );
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

    it("should create the repeat password validator", function () {
      var component = renderComponent();
      assert.instanceOf(
        component._repeatPasswordValidation, Validation.PasswordEqValidation
      );
      assert.equal(
        component._repeatPasswordValidation.get("with"),
        component.repeatPasswordComparator
      );
    });
  });

  it("should configure the terms link when it mounts", function () {
    var component = renderComponent();

    var $link = jQuery("a", component.refs.acceptTOSLabel.getDOMNode());
    assert.isTrue($link.hasClass("clickable"));
    assert.equal($link.attr("target"), "_blank");
    assert.equal($link.attr("href"), "/terms");
  });

  describe("fullName", function () {
    it("should not include second name if it's empty", function () {
      var component = renderComponent({sndname: ""});

      var result = component.fullName();
      assert.equal(result, "Spam");
    });

    it("should include second name if it isn't empty", function () {
      var component = renderComponent();

      var result = component.fullName();
      assert.equal(result, "Spam Eggs");
    });
  });

  it("should return the password for repeat password validator", function () {
    var component = renderComponent();
    component.setState({password: "spam123"});

    var result = component.repeatPasswordComparator();
    assert.equal(result, "spam123");
  });

  describe("validateAll", function () {
    it("should validate the full name", function () {
      var component = renderComponent();
      sinon.stub(component._fullNameValidation, "validateData");

      component.validateAll();
      assert(component._fullNameValidation.validateData.calledWith(
        component.state.fullName
      ));
    });

    it("should validate the password", function () {
      var component = renderComponent();
      sinon.stub(component._passwordValidation, "validateData");

      component.validateAll();
      assert(component._passwordValidation.validateData.calledWith(
        component.state.password
      ));
    });

    it("should validate the repeated password", function () {
      var component = renderComponent();
      sinon.stub(component._repeatPasswordValidation, "validateData");

      component.validateAll();
      assert(component._repeatPasswordValidation.validateData.calledWith(
        component.state.repeatPassword
      ));
    });

    it("should update state with the validation result", function () {
      var component = renderComponent();
      component.setState({
        fullName: "",
        password: "",
        repeatPassword: "spam123"
      });

      component.validateAll();
      assert.isFalse(component.state.fullNameValid);
      assert.isFalse(component.state.passwordValid);
      assert.isFalse(component.state.repeatPasswordValid);
    });

    it("should return false if any of the fields is invalid", function () {
      var component = renderComponent();
      component.setState({
        fullName: "",
        password: "password1234",
        repeatPassword: "password1234"
      });

      var result = component.validateAll();
      assert.isFalse(result);
    });

    it("should return true if all the fields are valid", function () {
      var component = renderComponent();
      component.setState({
        fullName: "Spam Eggs",
        password: "password1234",
        repeatPassword: "password1234"
      });

      var result = component.validateAll();
      assert.isTrue(result);
    });
  });

  it("should set password error mesage when validation fails", function () {
    var component = renderComponent();

    component.onPasswordValidationFail(
      "spam123", null, component._passwordValidation
    );
    assert.equal(
      component.state.passwordError, component._passwordValidation.message()
    );
  });

  it("should update state when the full name input changes", function () {
    var component = renderComponent();

    component.onFullNameInputChange("spam");
    assert.equal(component.state.fullName, "spam");
  });

  it("should update state when the company input changes", function () {
    var component = renderComponent();

    component.onCompanyInputChange("spam");
    assert.equal(component.state.company, "spam");
  });

  it("should update state when the position input changes", function () {
    var component = renderComponent();

    component.onPositionInputChange("spam");
    assert.equal(component.state.position, "spam");
  });

  it("should update state when the phone input changes", function () {
    var component = renderComponent();

    component.onPhoneInputChange("spam");
    assert.equal(component.state.phone, "spam");
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

  it("should toggle the accept TOS state when the accept TOS checkbox is clicked", function () {
    var component = renderComponent();

    component.onTOSCheckboxClick();
    assert.isTrue(component.state.acceptTOS);

    component.onTOSCheckboxClick();
    assert.isFalse(component.state.acceptTOS);
  });

  describe("onCreateAccountButtonClick", function () {
    var fakeSubmit = null;

    beforeEach(function () {
      fakeSubmit = new Submits.Submit();
      sinon.stub(fakeSubmit, "send");

      sinon.stub(Submits, "Submit").returns(fakeSubmit);
    });

    afterEach(function () {
      Submits.Submit.restore();
    });

    it("should update state to clear field validation errors", function () {
      var component = renderComponent();
      sinon.stub(component, "validateAll");
      component.setState({
        fullNameValid: false,
        passwordValid: false,
        passwordError: "spam",
        repeatPasswordValid: false
      });

      component.onCreateAccountButtonClick();
      assert.isTrue(component.state.fullNameValid);
      assert.isTrue(component.state.passwordValid);
      assert.equal(component.state.passwordError, "");
      assert.isTrue(component.state.repeatPasswordValid);
    });

    it("should block when accept TOS checkbox is unchecked", function () {
      var component = renderComponent();
      sinon.stub(component, "validateAll");
      component.setState({acceptTOS: false});

      component.onCreateAccountButtonClick();
      assert.isFalse(component.validateAll.called);
      assert.isFalse(Submits.Submit.calledWithNew());
    });

    it("should block when fields aren't invalid", function () {
      var component = renderComponent();
      sinon.stub(component, "validateAll").returns(false);
      component.setState({acceptTOS: true});

      component.onCreateAccountButtonClick();
      assert.isTrue(component.validateAll.called);
      assert.isFalse(Submits.Submit.calledWithNew());
    });

    it("should configure and send account setup request", function () {
      var component = renderComponent();
      component.setState({
        password: "password1234",
        repeatPassword: "password1234",
        acceptTOS: true
      });

      component.onCreateAccountButtonClick();
      assert.isTrue(Submits.Submit.calledWithNew());
      assert.isTrue(Submits.Submit.calledWith({
        url: "/accountsetup/1/eggsspam/CompanyInvitation",
        method: "POST",
        ajax: true,
        tos: "on",
        fstname: "Spam",
        sndname: "Eggs",
        password: "password1234",
        password2: "password1234",
        phone: "+48123456789",
        company: "Spameggs",
        position: "CEO",
        ajaxsuccess: component.onSubmitSuccess
      }));
      assert.isTrue(fakeSubmit.send.called);
    });
  });

  describe("render", function () {
    it("should configure and render the full name input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.fullNameInput);
      assert.equal(component.refs.fullNameInput.props.inputtype, "text");
      assert.equal(
        component.refs.fullNameInput.props.value, component.state.fullName
      );
      assert.equal(
        component.refs.fullNameInput.props.onChange,
        component.onFullNameInputChange
      );
    });

    it("should not render full name validation error message if the field is valid", function () {
      var component = renderComponent();
      component.setState({fullNameValid: true});

      assert.isUndefined(component.refs.fullNameValidationError);
    });

    it("should render full name validation error message if the field is invalid", function () {
      var component = renderComponent();
      component.setState({fullNameValid: false});

      assert.isDefined(component.refs.fullNameValidationError);
      assert.equal(
        component.refs.fullNameValidationError.getDOMNode().innerText,
        localization.accountSetupModal.modalAccountSetupMissingName
      );
    });

    it("should configure and render the company input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.companyInput);
      assert.equal(component.refs.companyInput.props.inputtype, "text");
      assert.isTrue(component.refs.companyInput.props.readonly);
      assert.equal(
        component.refs.companyInput.props.value, component.state.company
      );
      assert.equal(
        component.refs.companyInput.props.onChange,
        component.onCompanyInputChange
      );
    });

    it("should render the company input as editable if the user is company admin", function () {
      var component = renderComponent({companyAdmin: true});
      assert.isFalse(component.refs.companyInput.props.readonly);
    });

    it("should configure and render the position input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.positionInput);
      assert.equal(component.refs.positionInput.props.inputtype, "text");
      assert.equal(
        component.refs.positionInput.props.value, component.state.position
      );
      assert.equal(
        component.refs.positionInput.props.onChange,
        component.onPositionInputChange
      );
    });

    it("should configure and render the phone input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.phoneInput);
      assert.equal(component.refs.phoneInput.props.inputtype, "text");
      assert.equal(
        component.refs.phoneInput.props.value, component.state.phone
      );
      assert.equal(
        component.refs.phoneInput.props.onChange,
        component.onPhoneInputChange
      );
    });

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

    it("should not render password validation error message if the field is valid", function () {
      var component = renderComponent();
      component.setState({passwordValid: true});

      assert.isUndefined(component.refs.passwordValidationError);
    });

    it("should render password validation error message if the field is invalid", function () {
      var component = renderComponent();
      component.setState({passwordValid: false, passwordError: "spam"});

      assert.isDefined(component.refs.passwordValidationError);
      assert.equal(
        component.refs.passwordValidationError.getDOMNode().innerText, "spam"
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

    it("should not render repeat password validation error message if the field is valid", function () {
      var component = renderComponent();
      component.setState({repeatPasswordValid: true});

      assert.isUndefined(component.refs.repeatPasswordValidationError);
    });

    it("should render password validation error message if the field is invalid", function () {
      var component = renderComponent();
      component.setState({repeatPasswordValid: false});

      assert.isDefined(component.refs.repeatPasswordValidationError);
      assert.equal(
        component.refs.repeatPasswordValidationError.getDOMNode().innerText,
        localization.validation.passwordsDontMatch
      );
    });

    it("should configure and render the accept TOS checkbox", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.acceptTOSCheckbox);
      assert.equal(
        component.refs.acceptTOSCheckbox.props.onClick,
        component.onTOSCheckboxClick
      );
      assert.isTrue(
        component.refs.acceptTOSCheckbox.getDOMNode().classList.contains("checkbox")
      );
    });

    it("should render the accept TOS checkbox as checked if accepted TOS state is true", function () {
      var component = renderComponent();
      component.setState({acceptTOS: true});

      assert.isDefined(component.refs.acceptTOSCheckbox);
      assert.isTrue(
        component.refs.acceptTOSCheckbox.getDOMNode().classList.contains("checked")
      );
    });

    it("should render the accept TOS checkbox as unchecked if accepted TOS state is false", function () {
      var component = renderComponent();
      component.setState({acceptTOS: false});

      assert.isDefined(component.refs.acceptTOSCheckbox);
      assert.isFalse(
        component.refs.acceptTOSCheckbox.getDOMNode().classList.contains("checked")
      );
    });

    it("should configure and render the accept TOS checkbox label", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.acceptTOSLabel);
      assert.notEqual(component.refs.acceptTOSLabel.props.secureText, "");
      assert.equal(
        component.refs.acceptTOSLabel.props.onClicks["label"], component.onTOSCheckboxClick
      );
    });

    it("should not render repeat accept TOS validation error message if the field is valid", function () {
      var component = renderComponent();
      component.setState({acceptTOSValid: true});

      assert.isUndefined(component.refs.acceptTOSValidationError);
    });

    it("should render accept TOS validation error message if the field is invalid", function () {
      var component = renderComponent();
      component.setState({acceptTOSValid: false});

      assert.isDefined(component.refs.acceptTOSValidationError);
      assert.equal(
        component.refs.acceptTOSValidationError.getDOMNode().innerText,
        localization.validation.mustAcceptTOS
      );
    });

    it("should configure and render the create account button", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.createAccountButton);
      assert.equal(
        component.refs.createAccountButton.props.onClick,
        component.onCreateAccountButtonClick
      );
    });
  });
});
