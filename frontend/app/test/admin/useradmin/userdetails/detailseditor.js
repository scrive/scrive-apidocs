var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var DetailsEditorView = require(
  "../../../../scripts/admin/useradmin/userdetails/detailseditor"
);
var UserDetailsViewModel = require(
  "../../../../scripts/admin/useradmin/userdetails/userdetailsviewmodel"
);

describe("admin/useradmin/userdetails/detailseditor", function () {
  var container = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn(
      {
        userId: "userId",
        fstname: "fstname",
        sndname: "sndname",
        personalnumber: "personalnumber",
        email: "email",
        phone: "phone",
        lang: "en",
        companyposition: "companyposition",
        companyname: "companyname",
        companyid: "companyid",
        accountType: UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT,
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

  it("should render the readonly user ID field", function () {
    var component = renderComponent();

    var input = $("input[name=userId]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.isDefined(input.attr("readonly"));
    assert.equal(input.val(), component.props.userId);
  });

  it("should render the first name field", function () {
    var component = renderComponent();

    var input = $("input[name=fstname]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.fstname);
  });

  it("should propagate change of the first name field", function () {
    var component = renderComponent();

    var input = $("input[name=fstname]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("fstname", "changed")
    );
  });

  it("should render the second name field", function () {
    var component = renderComponent();

    var input = $("input[name=sndname]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.sndname);
  });

  it("should propagate change of the second name field", function () {
    var component = renderComponent();

    var input = $("input[name=sndname]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("sndname", "changed")
    );
  });

  it("should render the personal number field", function () {
    var component = renderComponent();

    var input = $("input[name=personalnumber]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.personalnumber);
  });

  it("should propagate change of the personal number field", function () {
    var component = renderComponent();

    var input = $("input[name=personalnumber]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("personalnumber", "changed")
    );
  });

  it("should render the email field", function () {
    var component = renderComponent();

    var input = $("input[name=email]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.email);
  });

  it("should propagate change of the email field", function () {
    var component = renderComponent();

    var input = $("input[name=email]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("email", "changed")
    );
  });

  it("should render the phone field", function () {
    var component = renderComponent();

    var input = $("input[name=phone]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.phone);
  });

  it("should propagate change of the phone field", function () {
    var component = renderComponent();

    var input = $("input[name=phone]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("phone", "changed")
    );
  });

  it("should render the company position field", function () {
    var component = renderComponent();

    var input = $("input[name=companyposition]", component.getDOMNode());

    assert.lengthOf(input, 1);
    assert.equal(input.val(), component.props.companyposition);
  });

  it("should propagate change of the company position field", function () {
    var component = renderComponent();

    var input = $("input[name=companyposition]", component.getDOMNode());
    input.val("changed");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith("companyposition", "changed")
    );
  });

  it("should render the language select", function () {
    var component = renderComponent();

    var select = $("div.user-details-select-lang", component.getDOMNode());
    assert.lengthOf(select, 1);

    var input = $("select", select);
    assert.equal(input.val(), "3");
  });

  it("should propagate change of the language select", function () {
    var component = renderComponent();

    var input = $(
      "div.user-details-select-lang select", component.getDOMNode()
    );

    input.val("2");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(component.props.onFieldChange.calledWith("lang", "nl"));
  });

  it("should render the company link", function () {
    var component = renderComponent();

    var link = $("a.user-details-company-link", component.getDOMNode());

    assert.lengthOf(link, 1);
    assert.equal(link.attr("href"), "/adminonly-old/companyadmin/companyid");
    assert.equal(link.text(), "Link to company companyname");
  });

  it("should render the account type select", function () {
    var component = renderComponent();

    var select = $(
      "div.user-details-select-account-type", component.getDOMNode()
    );
    
    assert.lengthOf(select, 1);

    var input = $("select", select);
    assert.equal(input.val(), "0");
  });

  it("should propagate change of the account type select", function () {
    var component = renderComponent();

    var input = $(
      "div.user-details-select-account-type select", component.getDOMNode()
    );

    input.val("1");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(
      component.props.onFieldChange.calledWith(
        "accountType", UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ADMIN
      )
    );
  });
});
