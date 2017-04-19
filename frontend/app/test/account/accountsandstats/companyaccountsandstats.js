var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var CompanyAccountsAndStatsView = require(
  "../../../scripts/account/usersandstats/companyaccountsandstats"
);
var CompanyAccountsTable = require(
  "../../../scripts/account/usersandstats/companyaccountstable"
);
var StatsView = require("../../../scripts/stats/stats");

describe("account/usersandstats/companyaccountsandstats", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || CompanyAccountsAndStatsView;

    var actualProps = underscore.extendOwn(
      {
        companyAdmin: false
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

    assert.equal(
      $("li:nth-child(1)", tabs).text(),
      localization.account.companyAccounts.name
    );
    assert.equal(
      $("li:nth-child(2)", tabs).text(),
      localization.account.companyAccounts.name
    );
    assert.equal(
      $("li:nth-child(3)", tabs).text(),localization.account.stats.name
    );
  });

  it("should activate the accounts tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#company-accounts");
  });

  it("should activate the company  stats tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#company-stats");
        done(); 
      }
    );
  });

  it("should configure and render the accounts view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var detailsView = TestUtils.findRenderedComponentWithType(
          component, CompanyAccountsTable
        );

        assert.isFalse(detailsView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the stats view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var statsView = TestUtils.findRenderedComponentWithType(
          component, StatsView
        );

        assert.isTrue(statsView.props.withCompany);

        done();
      }
    );
  });
});
