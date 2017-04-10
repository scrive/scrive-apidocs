var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../../../backend");
var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var DetailsEditorView = require(
  "../../../../scripts/admin/companyadmin/companydetails/detailseditor"
);

var ButtonBarView = require(
  "../../../../scripts/admin/companyadmin/companydetails/buttonbar"
);

var CompanyDetailsView = require(
  "../../../../scripts/admin/companyadmin/companydetails/companydetails"
).CompanyDetailsView;

var CompanyDetailsViewModel = require(
  "../../../../scripts/admin/companyadmin/companydetails/companydetailsviewmodel"
);

var CompanyModel = require("../../../../js/account/company.js").Company;

describe("admin/companyadmin/companydetails/companydetails", function () {
  var container = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || CompanyDetailsView;

    var actualProps = _.extendOwn(
      {
        companyId: "1"
      },
      props || {}
    );

    if (!actualProps.company) {
      actualProps.company = new CompanyModel({
        "id": "1",
        forAdmin: true
      });

      actualProps.company.on = sinon.stub();
      actualProps.company.off = sinon.stub();
      actualProps.company.set = sinon.stub();
      actualProps.company.fetch = sinon.stub();

      actualProps.company.companyname = sinon.stub().returns("name");
      actualProps.company.companynumber = sinon.stub().returns("number");
      actualProps.company.address = sinon.stub().returns("address");
      actualProps.company.zip = sinon.stub().returns("zip");
      actualProps.company.city = sinon.stub().returns("city");
      actualProps.company.country = sinon.stub().returns("country");
      actualProps.company.ipaddressmasklist = sinon.stub().returns(
        "ipaddressmasklist"
      );
      actualProps.company.cgidisplayname = sinon.stub().returns("cgidisplayname");
      actualProps.company.cgiserviceid = sinon.stub().returns("cgiserviceid");
      actualProps.company.idledoctimeout = sinon.stub().returns(99);
      actualProps.company.allowsavesafetycopy = sinon.stub().returns(true);
      actualProps.company.smsprovider = sinon.stub().returns("SMSDefault");
      actualProps.company.padappmode = sinon.stub().returns("list_view");
      actualProps.company.padearchiveenabled = sinon.stub().returns(true);

      actualProps.company.ready = sinon.stub().returns(false);
    }

    if (!actualProps.viewModel) {
      actualProps.viewModel = new CompanyDetailsViewModel();
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

  it("should subscribe to companys's change event when it mounts", function () {
    var component = renderComponent();
    assert.isTrue(component.props.company.on.calledWith(
      "change", component.onCompanyChange
    ));
  });

  it("should reload data when it mounts", function () {
    // This is a dirty hack and can break in the future if we upgrade to
    // newer React version.
    var fakeReloadData = sinon.stub(
      CompanyDetailsView.prototype.__reactAutoBindMap, "reloadData"
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

  it("should unsubscribe from companys's change event when it unmounts", function () {
    var component = renderComponent();
    React.unmountComponentAtNode(container);

    assert.isTrue(component.props.company.off.calledWith(
      "change", component.onCompanyChange
    ));
  });

  it("should reload data", function () {
    var component = renderComponent();

    component.props.company.set.reset();
    component.props.company.fetch.reset();
    component.reloadData();

    assert.isTrue(component.props.company.set.called);

    var setCall = component.props.company.set.firstCall;
    assert.isFalse(setCall.args[0].ready);
    assert.isTrue(setCall.args[1].silent);

    assert.isTrue(component.props.company.fetch.called);

    var fetchCall = component.props.company.fetch.firstCall;
    assert.isFalse(fetchCall.args[0].cache);
    assert.isTrue(fetchCall.args[0].processData);
  });

  it("should populate view model when company model changes", function () {
    var component = renderComponent();

    component.onCompanyChange();

    assert.isTrue(component.props.viewModel.set.called);

    var setCall = component.props.viewModel.set.firstCall;
    assert.equal(setCall.args[0].companyId, "1");
    assert.equal(setCall.args[0].name, "name");
    assert.equal(setCall.args[0].number, "number");
    assert.equal(setCall.args[0].address, "address");
    assert.equal(setCall.args[0].zip, "zip");
    assert.equal(setCall.args[0].city, "city");
    assert.equal(setCall.args[0].country, "country");
    assert.equal(setCall.args[0].ipaddressmasklist, "ipaddressmasklist");
    assert.equal(setCall.args[0].cgidisplayname, "cgidisplayname");
    assert.equal(setCall.args[0].cgiserviceid, "cgiserviceid");
    assert.equal(setCall.args[0].idledoctimeout, 99);
    assert.equal(setCall.args[0].allowsavesafetycopy, true);
    assert.equal(setCall.args[0].smsprovider, "SMSDefault");
    assert.equal(setCall.args[0].padappmode, "list_view");
    assert.equal(setCall.args[0].padearchiveenabled, true);

    assert.isTrue(setCall.args[1].silent);
  });

  it("should update ready state when company model changes", function () {
    var component = renderComponent();
    component.props.company.ready = sinon.stub().returns(true);

    component.onCompanyChange();

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

    component.onDetailsEditorFieldChange("name", "changed");
    assert.isTrue(component.props.viewModel.set.calledWith("name", "changed"));
  });

  xit("should handle merging to other company success", function (done) {
    sinon.stub(window.location, "assign");

    var component = renderComponent();
    component.onMergeSuccess("2");

    util.waitUntil(
      function () {
        return $(".flash.success").length == 1;
      },
      function () {
        assert.include($(".flash.success").text(), "Merged");
        assert.isTrue(window.location.assign.calledWith(
          "/adminonly/companyadmin/2"
        ));

        done();
      }
    );
  });

  it("should display error message when merging to other company fails", function (done) {
    var component = renderComponent();
    component.onMergeError();

    util.waitUntil(
      function () {
        return $(".flash.error").length == 1;
      },
      function () {
        assert.include($(".flash.error").text(), "Failed");
        done();
      }
    );
  });

  it("should merge company to other company", function () {
    var fakeSubmit = {sendAjax: sinon.stub()};

    var component = renderComponent();
    component.props.viewModel.mergeToCompany = sinon.stub().returns(
      fakeSubmit
    );

    component.onMerge("2");
    assert.isTrue(component.props.viewModel.mergeToCompany.calledWith("2"));
    assert.isTrue(fakeSubmit.sendAjax.calledWith(
      sinon.match.func, component.onMergeError
    ));
  });

  it("should display success message after saving details", function (done) {
    var component = renderComponent();
    component.onSaveComplete();

    util.waitUntil(
      function () {
        return $(".flash.success").length == 1;
      },
      function () {
        assert.include($(".flash.success").text(), "Saved");
        done();
      }
    );
  });

  it("should reload data after saving details", function () {
    var component = renderComponent();
    component.reloadData = sinon.stub();

    component.onSaveComplete({changed: true});
    assert.isTrue(component.reloadData.called);
  });

  it("should save company details", function () {
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
    var viewModel = new CompanyDetailsViewModel({
      companyId: "1",
      name: "name",
      number: "number",
      address: "address",
      zip: "zip",
      city: "city",
      country: "country",
      ipaddressmasklist: "ipaddressmasklist",
      cgidisplayname: "cgidisplayname",
      cgiserviceid: "cgiserviceid",
      idledoctimeout: 99,
      allowsavesafetycopy: true,
      smsprovider: "SMSDefault",
      padappmode: "list_view",
      padearchiveenabled: true
    });

    var component = renderComponent({viewModel: viewModel});
    component.setState({ready: true});

    var detailsEditor = TestUtils.findRenderedComponentWithType(
      component, DetailsEditorView
    );

    assert.equal(detailsEditor.props.companyId, "1");
    assert.equal(detailsEditor.props.companyId, "1");
    assert.equal(detailsEditor.props.name, "name");
    assert.equal(detailsEditor.props.number, "number");
    assert.equal(detailsEditor.props.address, "address");
    assert.equal(detailsEditor.props.zip, "zip");
    assert.equal(detailsEditor.props.city, "city");
    assert.equal(detailsEditor.props.country, "country");
    assert.equal(detailsEditor.props.ipaddressmasklist, "ipaddressmasklist");
    assert.equal(detailsEditor.props.cgidisplayname, "cgidisplayname");
    assert.equal(detailsEditor.props.cgiserviceid, "cgiserviceid");
    assert.equal(detailsEditor.props.idledoctimeout, 99);
    assert.equal(detailsEditor.props.allowsavesafetycopy, true);
    assert.equal(detailsEditor.props.smsprovider, "SMSDefault");
    assert.equal(detailsEditor.props.padappmode, "list_view");
    assert.equal(detailsEditor.props.padearchiveenabled, true);
    assert.equal(
      detailsEditor.props.onFieldChange, component.onDetailsEditorFieldChange
    );
  });

  it("should configure and render the button bar", function () {
    var viewModel = new CompanyDetailsViewModel({
      companyId: "1",
      name: "name",
      number: "number",
      address: "address",
      zip: "zip",
      city: "city",
      country: "country",
      ipaddressmasklist: "ipaddressmasklist",
      cgidisplayname: "cgidisplayname",
      cgiserviceid: "cgiserviceid",
      idledoctimeout: 99,
      allowsavesafetycopy: true,
      smsprovider: "SMSDefault",
      padappmode: "list_view",
      padearchiveenabled: true
    });

    var component = renderComponent({viewModel: viewModel});
    component.setState({ready: true});

    var buttonBar = TestUtils.findRenderedComponentWithType(
      component, ButtonBarView
    );

    assert.equal(buttonBar.props.companyId, "1");
    assert.equal(buttonBar.props.onMerge, component.onMerge);
    assert.equal(buttonBar.props.onSave, component.onSave);
  });
});
