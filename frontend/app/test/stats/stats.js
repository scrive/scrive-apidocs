var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var StatsTableView = require("../../scripts/stats/statstable");
var StatsView = require("../../scripts/stats/stats");

describe("stats/stats", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || StatsView;

    var actualProps = underscore.extendOwn(
      {
        withCompany: false
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
  });

  it("should generate base URL for user admin view", function () {
    var component = renderComponent({userId: "1"});

    var baseUrl = component.baseUrl();
    assert.equal(baseUrl, "/adminonly/useradmin/usagestats");
  });

  it("should generate base URL for company admin view", function () {
    var component = renderComponent({companyId: "1"});

    var baseUrl = component.baseUrl();
    assert.equal(baseUrl, "/adminonly/companyadmin/usagestats");
  });

  it("should generate base URL for account view", function () {
    var component = renderComponent();

    var baseUrl = component.baseUrl();
    assert.equal(baseUrl, "/api/frontend/usagestats");
  });

  it("should append user ID to URL for user admin view", function () {
    var component = renderComponent({userId: "userId"});

    var result = component.enrichUrl("/stats");
    assert.equal(result, "/stats/userId");
  });

  it("should append company ID to URL for company admin view", function () {
    var component = renderComponent({companyId: "companyId"});

    var result = component.enrichUrl("/stats");
    assert.equal(result, "/stats/companyId");
  });

  it("should add withCompany qs argument to company stats URL", function () {
    var component = renderComponent({withCompany: true});

    var result = component.enrichUrl("/stats");
    assert.equal(result, "/stats?withCompany=true")
  });

  it("should not add withCompany qs argument to user stats URL", function () {
    var component = renderComponent({withCompany: false});

    var result = component.enrichUrl("/stats");
    assert.equal(result, "/stats")
  });

  it("should generate URL for last 30 days stats", function () {
    var component = renderComponent();

    var result = component.urlForScope("days");
    assert.equal(result, "/api/frontend/usagestats/days");
  });

  it("should generate URL for last 6 months stats", function () {
    var component = renderComponent();

    var result = component.urlForScope("months");
    assert.equal(result, "/api/frontend/usagestats/months");
  });

  it("should configure and render stats table for the last 30 days stats", function () {
    var component = renderComponent();

    var statsViews = TestUtils.scryRenderedComponentsWithType(
      component, StatsTableView
    );

    assert.lengthOf(statsViews, 2);

    var last30DaysStatsView = statsViews[0];
    assert.isFalse(last30DaysStatsView.props.withCompany);
    assert.equal(
      last30DaysStatsView.props.url,
      "/api/frontend/usagestats/days"
    );
  });

  it("should configure and render stats table for the last 6 months stats", function () {
    var component = renderComponent();

    var statsViews = TestUtils.scryRenderedComponentsWithType(
      component, StatsTableView
    );

    assert.lengthOf(statsViews, 2);

    var last30DaysStatsView = statsViews[0];
    assert.isFalse(last30DaysStatsView.props.withCompany);
    assert.equal(
      last30DaysStatsView.props.url,
      "/api/frontend/usagestats/days"
    );
  });
});
