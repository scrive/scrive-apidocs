var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var CompanyAdminView = require(
  "../../../scripts/admin/companyadmin/companyadmin"
);

var CompanyBrandingPanel = require(
  "../../../scripts/account/branding/companybrandingpanel"
);

var CompanyDetailsView = require(
  "../../../scripts/admin/companyadmin/companydetails/companydetails"
).CompanyDetailsView;

var CompanyUsersAdminList = require(
  "../../../scripts/admin/companyusersadminlist"
);

var DocumentsList = require("../../../scripts/admin/documentslist");

var PaymentsPanel = require(
  "../../../scripts/admin/paymentspanel"
);

var StatsView = require("../../../scripts/stats/stats");

var TemplatesList = require("../../../scripts/admin/templateslist");

describe("admin/companyadmin/companyadmin", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || CompanyAdminView;

    var actualProps = _.extendOwn(
      {
        companyId: "1",
        forAdmin: true
      },
      props || {}
    );

    var component = React.render(
      React.createElement(componentClass, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();

    window.location.hash = "";
  });

  it("should render the tabs", function () {
    var component = renderComponent();

    var tabs = $(".tabs", component.getDOMNode());
    assert.lengthOf(tabs, 1);

    assert.equal($("li:nth-child(1)", tabs).text(), "<");
    assert.equal($("li:nth-child(2)", tabs).text(), "Company details");
    assert.equal($("li:nth-child(3)", tabs).text(), "Company users");
    assert.equal($("li:nth-child(4)", tabs).text(), "Branding");
    assert.equal($("li:nth-child(5)", tabs).text(), "Payments");
    assert.equal($("li:nth-child(6)", tabs).text(), "Statistics");
    assert.equal($("li:nth-child(7)", tabs).text(), "Templates");
    assert.equal($("li:nth-child(8)", tabs).text(), "Documents");
  });

  it("should activate the details tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#details");
  });

  it("should activate the users tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#users");
        done();
      }
    );
  });

  it("should activate the branding tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#branding");
        done();
      }
    );
  });

  it("should activate the payments tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(5)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#payments");
        done();
      }
    );
  });

  it("should activate the statistics tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(6)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#stats");
        done();
      }
    );
  });

  it("should activate the templates tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(7)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#templates");
        done();
      }
    );
  });

  it("should activate the documents tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(8)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#documents");
        done();
      }
    );
  });

  it("should configure and render the details view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var detailsView = TestUtils.findRenderedComponentWithType(
          component, CompanyDetailsView
        );

        assert.equal(detailsView.props.companyId, "1");

        done();
      }
    );
  });

  it("should configure and render the users view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var usersView = TestUtils.findRenderedComponentWithType(
          component, CompanyUsersAdminList
        );

        assert.equal(usersView.props.companyid, "1");
        assert.isFalse(usersView.props.loadLater, "1");

        done();
      }
    );
  });

  it("should configure and render the branding view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var brandingView = TestUtils.findRenderedComponentWithType(
          component, CompanyBrandingPanel
        );

        assert.equal(brandingView.props.companyid, "1");
        assert.isFalse(brandingView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the payments view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(5)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var paymentsView = TestUtils.findRenderedComponentWithType(
          component, PaymentsPanel
        );

        assert.equal(paymentsView.props.companyid, "1");
        assert.isFalse(paymentsView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the statistics view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(6)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var statisticsView = TestUtils.findRenderedComponentWithType(
          component, StatsView
        );

        assert.equal(statisticsView.props.companyId, "1");

        done();
      }
    );
  });

  it("should configure and render the templates view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(7)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var templatesView = TestUtils.findRenderedComponentWithType(
          component, TemplatesList
        );

        assert.isTrue(templatesView.props.forAdmin);
        assert.isFalse(templatesView.props.loadLater);
        assert.equal(templatesView.props.companyid, "1");

        done();
      }
    );
  });

  it("should configure and render the documents view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(8)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var documentsView = TestUtils.findRenderedComponentWithType(
          component, DocumentsList
        );

        assert.isTrue(documentsView.props.forAdmin);
        assert.isFalse(documentsView.props.loadLater);
        assert.equal(documentsView.props.companyid, "1");

        done();
      }
    );
  });
});
