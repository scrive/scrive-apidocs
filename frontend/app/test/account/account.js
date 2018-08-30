var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var AccountView = require("../../scripts/account/account");
var AccountSettingsPanel = require(
  "../../scripts/account/settings/accountsettingspanel"
);
var CompanyBrandingPanel = require(
  "../../scripts/account/branding/companybrandingpanel"
);
var CompanyAccountsAndStatsView = require(
  "../../scripts/account/usersandstats/companyaccountsandstats"
);
var APISettingsPanel = require(
  "../../scripts/account/apisettings/apisettingspanel"
);
var StatsView = require("../../scripts/stats/stats");
var SubscriptionPanel = require(
  "../../scripts/account/subscription/subscriptionpanel"
);

function findTab (name, component) {
  const lis = $(".tabs li", component.getDOMNode());
  var tab = null;

  lis.each(function (_, li) {
    if (li.innerText == name) {
      tab = li;
    }
  });

  return tab;
}

describe("account/account", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || AccountView;

    var actualProps = underscore.extendOwn(
      {
        companyAdmin: false,
        idledoctimeoutpreparation: 99,
        idledoctimeoutclosed: 98,
        idledoctimeoutcanceled: 97,
        idledoctimeouttimedout: 96,
        idledoctimeoutrejected: 95,
        idledoctimeouterror: 94,
        immediatetrash: true
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

  it("should render the tabs for admin user", function () {
    var component = renderComponent({companyAdmin: true});

    var tabs = $(".tabs", component.getDOMNode());
    assert.lengthOf(tabs, 1);

    var names =
      $("li", tabs).map(function (_, li) { return $(li).text(); })
      .toArray().sort();

    var expNames = [
      localization.account.accountDetails.name,
      localization.account.dataRetention.name,
      localization.account.companyAccounts.name,
      localization.account.companySettings,
      localization.account.apiSettings.name,
      localization.account.subscription
    ].sort();

    assert.deepEqual(names, expNames);
  });

  it("should render the tabs for non-admin user", function () {
    var component = renderComponent();

    var tabs = $(".tabs", component.getDOMNode());
    assert.lengthOf(tabs, 1);

    var names =
      $("li", tabs).map(function (_, li) { return $(li).text(); })
      .toArray().sort();

    var expNames = [
      localization.account.accountDetails.name,
      localization.account.dataRetention.name,
      localization.account.stats.name,
      localization.account.apiSettings.name
    ].sort();

    assert.deepEqual(names, expNames);
  });

  it("should activate the details tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(1)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#details");
  });

  it("should activate the data retention tab by clicking on it (admin)", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.dataRetention.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#data-retention");
        done();
      }
    );
  });

  it("should activate the data retention tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = findTab(localization.account.dataRetention.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#data-retention");
        done();
      }
    );
  });

  it("should activate the company accounts and stats tab by clicking on it", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.companyAccounts.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#company-accounts");
        done();
      }
    );
  });

  it("should activate the statistics tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = findTab(localization.account.stats.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#stats");
        done();
      }
    );
  });

  it("should activate the branding tab by clicking on it", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.companySettings, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#branding-themes-email");
        done();
      }
    );
  });

  it("should activate the API settings tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = findTab(localization.account.apiSettings.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#api-dashboard");
        done();
      }
    );
  });

  it("should activate the API settings tab by clicking on it (admin)", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.apiSettings.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#api-dashboard");
        done();
      }
    );
  });

  it("should activate the subscription tab by clicking on it", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.subscription, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#subscriptions");
        done();
      }
    );
  });

  it("should configure and render the details view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = findTab(localization.account.accountDetails.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var detailsView = TestUtils.findRenderedComponentWithType(
          component, AccountSettingsPanel
        );

        assert.isFalse(detailsView.props.companyAdmin);

        done();
      }
    );
  });

  it("should configure and render the company account and stats view when its tab is active", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.companyAccounts.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var accountsAndStatsView = TestUtils.findRenderedComponentWithType(
          component, CompanyAccountsAndStatsView
        );

        assert.isDefined(accountsAndStatsView);

        done();
      }
    );
  });

  it("should configure and render the statistics view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = findTab(localization.account.stats.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var statsView = TestUtils.findRenderedComponentWithType(
          component, StatsView
        );

        assert.isFalse(statsView.props.withCompany);

        done();
      }
    );
  });

  it("should configure and render the branding view when its tab is active", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.companySettings, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var brandingView = TestUtils.findRenderedComponentWithType(
          component, CompanyBrandingPanel
        );

        assert.isFalse(brandingView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the API settings view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = findTab(localization.account.apiSettings.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var apiSettingsView = TestUtils.findRenderedComponentWithType(
          component, APISettingsPanel
        );

        assert.isFalse(apiSettingsView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the API settings view when its tab is active (admin)", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.apiSettings.name, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var apiSettingsView = TestUtils.findRenderedComponentWithType(
          component, APISettingsPanel
        );

        assert.isFalse(apiSettingsView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the subscription view when its tab is active", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = findTab(localization.account.subscription, component);
    TestUtils.Simulate.click(tab);

    util.waitUntil(
      function () {
        return $(tab).hasClass("active");
      },
      function () {
        var subscriptionView = TestUtils.findRenderedComponentWithType(
          component, SubscriptionPanel
        );

        assert.isDefined(subscriptionView);

        done();
      }
    );
  });
});
