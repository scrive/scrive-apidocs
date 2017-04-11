var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var AdminView = require("../../scripts/admin/admin");
var BrandedDomainAdminPanel = require(
  "../../scripts/admin/brandeddomain/brandeddomainadminpanel"
);
var BrandedDomainsList = require("../../scripts/admin/brandeddomainslist");
var CompaniesAdminList = require("../../scripts/admin/companiesadminlist");
var DocumentsList = require("../../scripts/admin/documentslist");
var UsersAdminList = require("../../scripts/admin/usersadminlist");

describe("admin/admin", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || AdminView;

    var actualProps = _.extendOwn(
      {
        isAdmin: true
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

    assert.equal($("li:nth-child(1)", tabs).text(), "Sales user admin");
    assert.equal($("li:nth-child(2)", tabs).text(), "Company admin");
    assert.equal($("li:nth-child(3)", tabs).text(), "Documents");
    assert.equal($("li:nth-child(4)", tabs).text(), "Branded domains");
  });

  it("should activate the users tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(1)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#useradminforsales");
  });

  it("should activate the company tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#companyadmin");
        done(); 
      }
    );
  });

  it("should activate the documents tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
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

  it("should activate the branded domains tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#brandeddomains");
        done(); 
      }
    );
  });

  it("should configure and render the users view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(1)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var usersView = TestUtils.findRenderedComponentWithType(
          component, UsersAdminList
        );

        assert.isFalse(usersView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the companies view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var companiesView = TestUtils.findRenderedComponentWithType(
          component, CompaniesAdminList
        );

        assert.isFalse(companiesView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the documents view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
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

        done();
      }
    );
  });

  it("should configure and render the branded domains view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var brandedDomainsView = TestUtils.findRenderedComponentWithType(
          component, BrandedDomainsList
        );

        assert.isFalse(brandedDomainsView.props.loadLater);
        assert.equal(
          brandedDomainsView.props.onSelect, component.onBrandedDomainSelect
        );

        done();
      }
    );
  });

  it("should configure and render the branded domain view when a domain is selected", function (done) {
    var component = renderComponent();

    var usersTab = $(".tabs li:nth-child(1)", component.getDOMNode());

    component.onBrandedDomainSelect(999);

    util.waitUntil(
      function () {
        return (usersTab.hasClass("active") == false);
      },
      function () {
        var brandedDomainPanelView = TestUtils.findRenderedComponentWithType(
          component, BrandedDomainAdminPanel
        );

        assert.equal(brandedDomainPanelView.props.domainid, "999");

        done();
      }
    );
  });
});
