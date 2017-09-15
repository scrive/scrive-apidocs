var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var backend = require("../../backend");
var util = require("../../util");

var AccountSettingsModel = require(
  "../../../scripts/account/settings/accountsettings"
);
var ChangeEmailModal = require(
  "../../../scripts/account/settings/changeemailmodal"
);
var FlashMessage = require("../../../js/flashmessages.js");
var Track = require("../../../scripts/common/track");

describe("account/settings/changeemailmodal", function () {
  var container = null;
  var model = null;

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  beforeEach(function () {
    sinon.stub(FlashMessage, "FlashMessage");
    sinon.stub(Track, "track_timeout");

    model = new AccountSettingsModel();
    sinon.stub(model, "changeEmail");
    sinon.stub(model, "refresh");
    sinon.stub(model, "setNewEmail");
    sinon.stub(model, "setNewEmailAgain");
    sinon.stub(model, "updateProfile");
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    FlashMessage.FlashMessage.restore();
    Track.track_timeout.restore();

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
        accountSettings: model,
        onClose: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(ChangeEmailModal, actualProps), container
    );

    return component;
  };

  it("should initialize state", function () {
    var component = renderComponent();
    assert.equal(component.state.newEmail, "");
    assert.equal(component.state.newEmailAgain, "");
  });

  it("should clear state when it hides", function () {
    var component = renderComponent();
    component.setState({
      newEmail: "spam",
      newEmailAgain: "eggs"
    });

    component.onHide();
    assert.equal(component.state.newEmail, "");
    assert.equal(component.state.newEmailAgain, "");
  });

  it("should refresh the account settings when it hides", function () {
    var component = renderComponent();

    component.onHide();
    assert.isTrue(model.refresh.called);
  });

  it("should change the e-mail", function () {
    var component = renderComponent();
    component.setState({newEmail: "spam@example.com"});
    component.setState({newEmailAgain: "spam@example.com"});

    component.changeEmail();
    assert.isTrue(model.setNewEmail.calledWith("spam@example.com"));
    assert.isTrue(model.setNewEmailAgain.calledWith("spam@example.com"));
    assert.isTrue(model.changeEmail.calledWith(component.updateProfile));
  });

  it("should update the user profile", function () {
    var component = renderComponent();

    component.updateProfile();
    assert.isTrue(model.updateProfile.calledWith(component.props.onClose));
  });

  it("should display error message when trying to save an empty e-mail address", function () {
    var component = renderComponent();

    component.onAcceptButtonClick();
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.account.accountDetails.invalidEmail
    }));
  });

  it("should display error message when trying to save an invalid e-mail address", function () {
    var component = renderComponent();
    component.setState({newEmail: "spam"});

    component.onAcceptButtonClick();
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.account.accountDetails.invalidEmail
    }));
  });

  it("should display error message when trying to save and e-mail addresses aren't equal", function () {
    var component = renderComponent();
    component.setState({newEmail: "spam@example.com"});
    component.setState({newEmailAgain: "eggs@example.com"});

    component.onAcceptButtonClick();
    assert.isTrue(FlashMessage.FlashMessage.calledWithNew());
    assert.isTrue(FlashMessage.FlashMessage.calledWith({
      type: "error",
      content: localization.account.accountDetails.mismatchedEmails
    }));
  });

  it("should save the new e-mail addresses", function () {
    var component = renderComponent();
    component.setState({newEmail: "spam@example.com"});
    component.setState({newEmailAgain: "spam@example.com"});

    component.onAcceptButtonClick();    
    assert.isTrue(Track.track_timeout.calledWith(
      "Accept", {"Accept": "Change email"}, component.changeEmail
    ));
  });

  it("should update state when e-mail address field changes", function () {
    var component = renderComponent();

    component.onNewEmailChange({target: {value: "spam"}});
    assert.equal(component.state.newEmail, "spam");
  });

  it("should update state when repeat e-mail address field changes", function () {
    var component = renderComponent();

    component.onNewEmailAgainChange({target: {value: "spam"}});
    assert.equal(component.state.newEmailAgain, "spam");
  });

  it("should configure and render the e-mail field", function () {
    var component = renderComponent();
    component.setState({newEmail: "spam"});

    assert.isDefined(component.refs.newEmail);
    assert.equal(
      component.refs.newEmail.props.value, component.state.newEmail
    );
    assert.equal(
      component.refs.newEmail.props.onChange, component.onNewEmailChange
    );
  });

  it("should configure and render the repeat e-mail field", function () {
    var component = renderComponent();
    component.setState({newEmailAgain: "spam"});

    assert.isDefined(component.refs.newEmailAgain);
    assert.equal(
      component.refs.newEmailAgain.props.value, component.state.newEmailAgain
    );
    assert.equal(
      component.refs.newEmailAgain.props.onChange,
      component.onNewEmailAgainChange
    );
  })
});
