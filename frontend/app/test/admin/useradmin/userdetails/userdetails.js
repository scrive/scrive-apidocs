var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../../backend");
var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var DetailsEditorView = require(
  "../../../../scripts/admin/useradmin/userdetails/detailseditor"
);

var ButtonBarView = require(
  "../../../../scripts/admin/useradmin/userdetails/buttonbar"
);

var UserDetailsView = require(
  "../../../../scripts/admin/useradmin/userdetails/userdetails"
).UserDetailsView;

var UserDetailsViewModel = require(
  "../../../../scripts/admin/useradmin/userdetails/userdetailsviewmodel"
);

var UserModel = require("../../../../js/account/user.js").User;
var CompanyModel = require("../../../../js/account/company.js").Company;

describe("admin/useradmin/userdetails/userdetails", function () {
  var container = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || UserDetailsView;

    var actualProps = underscore.extendOwn(
      {
        userId: "1"
      },
      props || {}
    );

    if (!actualProps.user) {
      actualProps.user = new UserModel({
        id: "id",
        forAdmin: true
      });

      actualProps.user.on = sinon.stub();
      actualProps.user.off = sinon.stub();
      actualProps.user.set = sinon.stub();
      actualProps.user.fetch = sinon.stub();

      actualProps.user.fstname = sinon.stub().returns("fstname");
      actualProps.user.sndname = sinon.stub().returns("sndname");
      actualProps.user.personalnumber = sinon.stub().returns("personalnumber");
      actualProps.user.email = sinon.stub().returns("email");
      actualProps.user.phone = sinon.stub().returns("phone");
      actualProps.user.lang = sinon.stub().returns("lang");
      actualProps.user.companyposition = sinon.stub().returns(
        "companyposition"
      );
      actualProps.user.companyadmin = sinon.stub().returns(false);
      actualProps.user.ready = sinon.stub().returns(false);

      var company = new CompanyModel({
        companyid: "companyid",
        companyname: "companyname",
        entityname: "entityname"
      });
      actualProps.user.company = sinon.stub().returns(company);
    }

    if (!actualProps.viewModel) {
      actualProps.viewModel = new UserDetailsViewModel();
      actualProps.viewModel.on = sinon.stub();
      actualProps.viewModel.off = sinon.stub();
      actualProps.viewModel.set = sinon.stub();
    }

    var component = React.render(
      React.createElement(componentClass, actualProps), container
    );

    return component;
  };

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.ready);
  });

  it("should subscribe to view model's change event when it mounts", function () {
    var component = renderComponent();
    assert.isTrue(component.props.viewModel.on.calledWith(
      "change", component.onViewModelChange
    ));
  });

  it("should subscribe to users's change event when it mounts", function () {
    var component = renderComponent();
    assert.isTrue(component.props.user.on.calledWith(
      "change", component.onUserChange
    ));
  });

  it("should reload data when it mounts", function () {
    // This is a dirty hack and can break in the future if we upgrade to
    // newer React version.
    var fakeReloadData = sinon.stub(
      UserDetailsView.prototype.__reactAutoBindMap, "reloadData"
    );

    var component = renderComponent();
    assert.isTrue(fakeReloadData.called);

    fakeReloadData.restore();
  });

  it("should unsubscribe from view model's change event when it unmounts", function () {
    var component = renderComponent();
    React.unmountComponentAtNode(container);

    assert.isTrue(component.props.viewModel.off.calledWith(
      "change", component.onViewModelChange
    ));
  });

  it("should unsubscribe from users's change event when it unmounts", function () {
    var component = renderComponent();
    React.unmountComponentAtNode(container);

    assert.isTrue(component.props.user.off.calledWith(
      "change", component.onUserChange
    ));
  });

  it("should reload data", function () {
    var component = renderComponent();

    component.props.user.set.reset();
    component.props.user.fetch.reset();
    component.reloadData();

    assert.isTrue(component.props.user.set.called);

    var setCall = component.props.user.set.firstCall;
    assert.isFalse(setCall.args[0].ready);
    assert.isTrue(setCall.args[1].silent);

    assert.isTrue(component.props.user.fetch.called);

    var fetchCall = component.props.user.fetch.firstCall;
    assert.isFalse(fetchCall.args[0].cache);
    assert.isTrue(fetchCall.args[0].processData);
  });

  it("should populate view model when user model changes", function () {
    var component = renderComponent();

    component.onUserChange();

    assert.isTrue(component.props.viewModel.set.called);

    var setCall = component.props.viewModel.set.firstCall;
    assert.equal(setCall.args[0].userId, "1");
    assert.equal(setCall.args[0].fstname, "fstname");
    assert.equal(setCall.args[0].sndname, "sndname");
    assert.equal(setCall.args[0].personalnumber, "personalnumber");
    assert.equal(setCall.args[0].email, "email");
    assert.equal(setCall.args[0].phone, "phone");
    assert.equal(setCall.args[0].lang, "lang");
    assert.equal(setCall.args[0].companyposition, "companyposition");
    assert.equal(setCall.args[0].companyname, "entityname");
    assert.equal(setCall.args[0].companyid, "companyid");
    assert.equal(
      setCall.args[0].accountType,
      UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT
    );

    assert.isTrue(setCall.args[1].silent);
  });

  it("should set account type to admin if the user is admin", function () {
    var component = renderComponent();
    component.props.user.companyadmin = sinon.stub().returns(true);

    component.onUserChange();

    var setCall = component.props.viewModel.set.firstCall;
    assert.equal(
      setCall.args[0].accountType,
      UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ADMIN
    );
  });

  it("should update ready state when user model changes", function () {
    var component = renderComponent();
    component.props.user.ready = sinon.stub().returns(true);

    component.onUserChange();

    assert.isTrue(component.state.ready);
  });

  it("should update when view model changes", function () {
    var component = renderComponent();
    component.forceUpdate = sinon.stub();

    component.onViewModelChange();
    assert.isTrue(component.forceUpdate.called);
  });

  it("should update view model when details editor field changes", function () {
    var component = renderComponent();

    component.onDetailsEditorFieldChange("fstname", "changed");
    assert.isTrue(component.props.viewModel.set.calledWith(
      "fstname", "changed"
    ));
  });

  xit("should redirect to company users page after deleting user", function () {
    sinon.stub(window.location, "assign");

    var component = renderComponent();

    component.onDeleteSuccess();
    assert.isTrue(window.location.assign.calledWith(
      "/adminonly/companyadmin/companyid/#users"
    ));
  });

  it("should display error message when deleting user fails", function (done) {
    var component = renderComponent();
    component.onDeleteError();

    util.waitUntil(
      function () {
        return $(".flash .error").length == 1;
      },
      function () {
        assert.include($(".flash .error").text(), "Failed");
        done();
      }
    );
  });

  it("should delete the user", function () {
    var fakeSubmit = {sendAjax: sinon.stub()};

    var component = renderComponent();
    component.props.viewModel.deleteUser = sinon.stub().returns(fakeSubmit);

    component.onDelete();
    assert.isTrue(component.props.viewModel.deleteUser.called);
    assert.isTrue(fakeSubmit.sendAjax.calledWith(
      component.onDeleteSuccess, component.onDeleteError
    ));
  });

  it("should display success message after resending invitation", function (done) {
    var component = renderComponent();
    component.onResendInvitationComplete();

    util.waitUntil(
      function () {
        return $(".flash .success").length == 1;
      },
      function () {
        assert.include($(".flash .success").text(), "Invitation sent");
        done();
      }
    );
  });

  it("should reload data success message after resending invitation", function () {
    var component = renderComponent();
    component.reloadData = sinon.stub();

    component.onResendInvitationComplete();
    assert.isTrue(component.reloadData.called);
  });

  it("should resend invitaion", function () {
    var fakeSubmit = {sendAjax: sinon.stub()};

    var component = renderComponent();
    component.props.viewModel.resendInvitation = sinon.stub().returns(
      fakeSubmit
    );

    component.onResendInvitation();
    assert.isTrue(component.props.viewModel.resendInvitation.called);
    assert.isTrue(fakeSubmit.sendAjax.calledWith(
      component.onResendInvitationComplete
    ));
  });

  it("should display success message after moving to other company", function (done) {
    var component = renderComponent();
    component.onMoveSuccess();

    util.waitUntil(
      function () {
        return $(".flash .success").length == 1;
      },
      function () {
        assert.include($(".flash .success").text(), "Moved");
        done();
      }
    );
  });

  it("should reload data success message after moving to other company", function () {
    var component = renderComponent();
    component.reloadData = sinon.stub();

    component.onMoveSuccess();
    assert.isTrue(component.reloadData.called);
  });

  it("should display error message when moving to other company fails", function (done) {
    var component = renderComponent();
    component.onMoveError();

    util.waitUntil(
      function () {
        return $(".flash .error").length == 1;
      },
      function () {
        assert.include($(".flash .error").text(), "Failed");
        done();
      }
    );
  });

  it("should move user to other company", function () {
    var fakeSubmit = {sendAjax: sinon.stub()};

    var component = renderComponent();
    component.props.viewModel.moveToCompany = sinon.stub().returns(
      fakeSubmit
    );

    component.onMove("2");
    assert.isTrue(component.props.viewModel.moveToCompany.calledWith("2"));
    assert.isTrue(fakeSubmit.sendAjax.calledWith(
      component.onMoveSuccess, component.onMoveError
    ));
  });

  it("should display success message after saving details", function (done) {
    var component = renderComponent();
    component.onSaveComplete({changed: true});

    util.waitUntil(
      function () {
        return $(".flash .success").length == 1;
      },
      function () {
        assert.include($(".flash .success").text(), "Saved");
        done();
      }
    );
  });

  it("should reload data success message after saving details", function () {
    var component = renderComponent();
    component.reloadData = sinon.stub();

    component.onSaveComplete({changed: true});
    assert.isTrue(component.reloadData.called);
  });

  it("should display error message when saving details fails", function (done) {
    var component = renderComponent();
    component.onSaveComplete({changed: false});

    util.waitUntil(
      function () {
        return $(".flash .error").length == 1;
      },
      function () {
        assert.include(
          $(".flash .error").text(), "Failure. User already exists"
        );

        done();
      }
    );
  });

  it("should save user details", function () {
    var fakeSubmit = {sendAjax: sinon.stub()};

    var component = renderComponent();
    component.props.viewModel.saveDetails = sinon.stub().returns(
      fakeSubmit
    );

    component.onSave();
    assert.isTrue(fakeSubmit.sendAjax.calledWith(
      component.onSaveComplete
    ));
  });

  it("should not render when not ready", function () {
    var component = renderComponent();
    assert.lengthOf($(".tab-container.account", component.getDOMNode()), 0);
  });

  it("should render when ready", function () {
    var component = renderComponent();
    component.setState({ready: true});

    assert.lengthOf($(".tab-container.account", component.getDOMNode()), 1);
  });

  it("should configure and render the details editor", function () {
    var viewModel = new UserDetailsViewModel({
      userId: "1",
      fstname: "fstname",
      sndname: "sndname",
      personalnumber: "personalnumber",
      email: "email",
      phone: "phone",
      lang: "lang",
      companyposition: "companyposition",
      companyname: "companyname",
      companyid: "companyid",
      accountType: UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT
    });

    var component = renderComponent({viewModel: viewModel});
    component.setState({ready: true});

    var detailsEditor = TestUtils.findRenderedComponentWithType(
      component, DetailsEditorView
    );

    assert.equal(detailsEditor.props.userId, "1");
    assert.equal(detailsEditor.props.fstname, "fstname");
    assert.equal(detailsEditor.props.sndname, "sndname");
    assert.equal(detailsEditor.props.personalnumber, "personalnumber");
    assert.equal(detailsEditor.props.email, "email");
    assert.equal(detailsEditor.props.phone, "phone");
    assert.equal(detailsEditor.props.lang, "lang");
    assert.equal(detailsEditor.props.companyposition, "companyposition");
    assert.equal(detailsEditor.props.companyname, "companyname");
    assert.equal(detailsEditor.props.companyid, "companyid");
    assert.equal(
      detailsEditor.props.accountType,
      UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT
    );
    assert.equal(
      detailsEditor.props.onFieldChange, component.onDetailsEditorFieldChange
    );
  });

  it("should configure and render the button bar", function () {
    var viewModel = new UserDetailsViewModel({
      userId: "1",
      fstname: "fstname",
      sndname: "sndname",
      personalnumber: "personalnumber",
      email: "email",
      phone: "phone",
      lang: "lang",
      companyposition: "companyposition",
      companyname: "companyname",
      companyid: "companyid",
      accountType: UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT
    });

    var component = renderComponent({viewModel: viewModel});
    component.setState({ready: true});

    var buttonBar = TestUtils.findRenderedComponentWithType(
      component, ButtonBarView
    );

    assert.equal(buttonBar.props.companyid, "companyid");
    assert.equal(buttonBar.props.onDelete, component.onDelete);
    assert.equal(
      buttonBar.props.onResendInvitation, component.onResendInvitation
    );
    assert.equal(buttonBar.props.onMove, component.onMove);
    assert.equal(buttonBar.props.onSave, component.onSave);
  });
});
