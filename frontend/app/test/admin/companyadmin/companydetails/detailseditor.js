var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var DetailsEditorView = require(
  "../../../../scripts/admin/companyadmin/companydetails/detailseditor"
);
var CompanyDetailsViewModel = require(
  "../../../../scripts/admin/companyadmin/companydetails/companydetailsviewmodel"
);

describe("admin/companyadmin/companydetails/detailseditor", function () {
  var container = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = _.extendOwn(
      {
        companyId: "companyId",
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
        smsprovider: "SMSDefault",
        padappmode: "list_view",
        padearchiveenabled: true,
        onFieldChange: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(DetailsEditorView, actualProps), container
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

  it("should render the readonly company ID field", function () {
    var component = renderComponent();

    var input = $("input[name=companyId]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.isDefined(input.attr("readonly"));
    assert.equal(input.val(), component.props.companyId);
  });

  it("should render the name field", function () {
    var component = renderComponent();

    var input = $("input[name=name]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.name);
  });

  it("should propagate change of the name field", function () {
    var component = renderComponent();

    var input = $("input[name=name]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("name", "changed")
    );
  });

  it("should render the number field", function () {
    var component = renderComponent();

    var input = $("input[name=number]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.number);
  });

  it("should propagate change of the number field", function () {
    var component = renderComponent();

    var input = $("input[name=number]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("number", "changed")
    );
  });

  it("should render the address field", function () {
    var component = renderComponent();

    var input = $("input[name=address]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.address);
  });

  it("should propagate change of the address field", function () {
    var component = renderComponent();

    var input = $("input[name=address]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("address", "changed")
    );
  });

  it("should render the zip field", function () {
    var component = renderComponent();

    var input = $("input[name=zip]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.zip);
  });

  it("should propagate change of the zip field", function () {
    var component = renderComponent();

    var input = $("input[name=zip]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("zip", "changed")
    );
  });

  it("should render the city field", function () {
    var component = renderComponent();

    var input = $("input[name=city]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.city);
  });

  it("should propagate change of the city field", function () {
    var component = renderComponent();

    var input = $("input[name=city]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("city", "changed")
    );
  });

  it("should render the country field", function () {
    var component = renderComponent();

    var input = $("input[name=country]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.country);
  });

  it("should propagate change of the country field", function () {
    var component = renderComponent();

    var input = $("input[name=country]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("country", "changed")
    );
  });

  it("should render the IP address mask list field", function () {
    var component = renderComponent();

    var input = $("input[name=ipaddressmasklist]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.ipaddressmasklist);
  });

  it("should propagate change of the IP address mask list field", function () {
    var component = renderComponent();

    var input = $("input[name=ipaddressmasklist]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("ipaddressmasklist", "changed")
    );
  });

  it("should render the CGI display name field", function () {
    var component = renderComponent();

    var input = $("input[name=cgidisplayname]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.cgidisplayname);
  });

  it("should propagate change of the CGI display name field", function () {
    var component = renderComponent();

    var input = $("input[name=cgidisplayname]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("cgidisplayname", "changed")
    );
  });

  it("should render the CGI service ID field", function () {
    var component = renderComponent();

    var input = $("input[name=cgiserviceid]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.cgiserviceid);
  });

  it("should propagate change of the CGI service ID field", function () {
    var component = renderComponent();

    var input = $("input[name=cgiserviceid]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("cgiserviceid", "changed")
    );
  });

  it("should render the idle document timeout field", function () {
    var component = renderComponent();

    var input = $("input[name=idledoctimeout]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(
      input.attr("min"), CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MIN
    );
    assert.equal(
      input.attr("max"), CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MAX
    );
    assert.equal(input.attr("type"), "number");
    assert.equal(input.val(), component.props.idledoctimeout);
  });

  it("should propagate change of the idle document timeout field", function () {
    var component = renderComponent();

    var input = $("input[name=idledoctimeout]", component.getDOMNode());
    input.val("1");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("idledoctimeout", 1)
    );
  });

  it("should render the SMS provider select", function () {
    var component = renderComponent();

    var select = $(
      "div.company-details-select-sms-provider", component.getDOMNode()
    );
    assert.lengthOf(select, 1);

    var input = $("select", select);
    assert.equal(input.val(), "0");
  });

  it("should propagate change of the SMS provider select", function () {
    var component = renderComponent();

    var input = $(
      "div.company-details-select-sms-provider select", component.getDOMNode()
    );

    input.val("1");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(component.props.onFieldChange.calledWith(
      "smsprovider", "SMSTeliaCallGuide"
    ));
  });

  it("should render the padd application mode select", function () {
    var component = renderComponent();

    var select = $(
      "div.company-details-select-pad-app-mode", component.getDOMNode()
    );

    assert.lengthOf(select, 1);

    var input = $("select", select);
    assert.equal(input.val(), "0");
  });

  it("should propagate change of the padd application mode select", function () {
    var component = renderComponent();

    var input = $(
      "div.company-details-select-pad-app-mode select", component.getDOMNode()
    );

    input.val("1");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(component.props.onFieldChange.calledWith(
      "padappmode", "pin_code"
    ));
  });

  it("should render the enable e-archive in iPad app checkbox", function () {
    var component = renderComponent();

    var input = $("input[name=padearchiveenabled]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.attr("checked"), "checked");
  });

  it("should propagate change of the enable e-archive in iPad app checkbox", function () {
    var component = renderComponent();

    var input = $("input[name=padearchiveenabled]", component.getDOMNode());
    input.attr("checked", false);
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("padearchiveenabled", false)
    );
  });
});
