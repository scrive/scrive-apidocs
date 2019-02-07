var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var ShareableLinkStatsTableView = require("../../scripts/stats/shareablelinkstatstable");
var ShareableLinkStatsView = require("../../scripts/stats/shareablelinkstats");

describe("stats/shareablelinkstats", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || ShareableLinkStatsView;

    var actualProps = underscore.extendOwn(
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

  it("should generate base URL for company admin view", function () {
    var component = renderComponent({groupId: "1"});

    var baseUrl = component.baseUrl();
    assert.equal(baseUrl, "/adminonly/companyadmin/shareablelinkstats");
  });

  it("should generate base URL for user admin view", function () {
    var component = renderComponent({userId: "1"});

    var baseUrl = component.baseUrl();
    assert.equal(baseUrl, "/adminonly/useradmin/shareablelinkstats");
  });

  it("should generate base URL for account view", function () {
    var component = renderComponent();

    var baseUrl = component.baseUrl();
    assert.equal(baseUrl, "/api/v2/usagestats/shareablelink");
  });

  it("should append user ID to URL for user admin view", function () {
    var component = renderComponent({userId: "userId"});

    var result = component.enrichUrl("url");
    assert.equal(result, "url/userId");
  });

  it("should append group ID to URL for group admin view", function () {
    var component = renderComponent({groupId: "groupId"});

    var result = component.enrichUrl("url");
    assert.equal(result, "url/groupId");
  });

  it("should generate URL for last 30 days stats", function () {
    var component = renderComponent();

    var result = component.urlForScope("days");
    assert.equal(result, "/api/v2/usagestats/shareablelink/days");
  });

  it("should generate URL for last 6 months stats", function () {
    var component = renderComponent();

    var result = component.urlForScope("months");
    assert.equal(result, "/api/v2/usagestats/shareablelink/months");
  });

  it("should configure and render stats table for the last 30 days stats", function () {
    var component = renderComponent();

    var statsViews = TestUtils.scryRenderedComponentsWithType(
      component, ShareableLinkStatsTableView
    );

    assert.lengthOf(statsViews, 2);

    var last30DaysStatsView = statsViews[0];
    assert.equal(
      last30DaysStatsView.props.url,
      "/api/v2/usagestats/shareablelink/days"
    );
  });

  it("should configure and render stats table for the last 6 months stats", function () {
    var component = renderComponent();

    var statsViews = TestUtils.scryRenderedComponentsWithType(
      component, ShareableLinkStatsTableView
    );

    assert.lengthOf(statsViews, 2);

    var last30DaysStatsView = statsViews[1];
    assert.equal(
      last30DaysStatsView.props.url,
      "/api/v2/usagestats/shareablelink/months"
    );
  });
});
