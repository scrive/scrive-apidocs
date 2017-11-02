var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var APILogView = require(
  "../../../scripts/account/apisettings/apilogview"
);
var APISettingsPanel = require(
  "../../../scripts/account/apisettings/apisettingspanel"
);
var OAuthDashboardView = require(
  "../../../scripts/account/apisettings/oauthdashboardview"
);

describe("account/apisettings/apisettingspanel", function () {
  var container = null;

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    server.restore();
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        loadLater: false
      },
      props || {}
    );

    var component = React.render(
      React.createElement(APISettingsPanel, actualProps), container
    );

    return component;
  };

  it("should render the tabs", function () {
    var component = renderComponent();

    var tabs = $(".tabs", component.getDOMNode());
    assert.lengthOf(tabs, 1);

    assert.equal(
      $("li:nth-child(2)", tabs).text(),
      localization.account.apiSettings.name
    );
    assert.equal(
      $("li:nth-child(3)", tabs).text(),
      localization.account.apiLog.name
    );
  });

  it("should activate the OAuth dashboard tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#api-dashboard");
  });

  it("should activate the API log tab by clicking on it", function (done) {
    var component = renderComponent({companyAdmin: true});

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#api-log");
        done(); 
      }
    );
  });

  it("should configure and render the OAuth dashboard view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var detailsView = TestUtils.findRenderedComponentWithType(
          component, OAuthDashboardView
        );

        assert.isFalse(detailsView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the API log view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var detailsView = TestUtils.findRenderedComponentWithType(
          component, APILogView
        );

        assert.isFalse(detailsView.props.loadLater);

        done();
      }
    );
  });
});
