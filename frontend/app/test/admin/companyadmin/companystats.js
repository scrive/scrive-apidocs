var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var CompanyStatsView = require(
  "../../../scripts/admin/companyadmin/companystats"
);

var StatsTableView = require(
  "../../../scripts/account/usersandstats/statstable"
);

describe("admin/companyadmin/companystats", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || CompanyStatsView;

    var actualProps = _.extendOwn(
      {
        companyId: "1"
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

  it("should configure and render the last 30 days stats view", function () {
    var component = renderComponent();

    var statsViews = TestUtils.scryRenderedComponentsWithType(
      component, StatsTableView
    );

    assert.lengthOf(statsViews, 2);

    var last30DaysStatsView = statsViews[0];
    assert.isTrue(last30DaysStatsView.props.withCompany);
    assert.equal(
      last30DaysStatsView.props.url,
      "/adminonly/companyadmin/usagestats/days/1?withCompany=true"
    );
  });

  it("should configure and render the last 6 months stats view", function () {
    var component = renderComponent();

    var statsViews = TestUtils.scryRenderedComponentsWithType(
      component, StatsTableView
    );

    assert.lengthOf(statsViews, 2);

    var last6MonthsStatsView = statsViews[1];
    assert.isTrue(last6MonthsStatsView.props.withCompany);
    assert.equal(
      last6MonthsStatsView.props.url,
      "/adminonly/companyadmin/usagestats/months/1?withCompany=true"
    );
  });
});
