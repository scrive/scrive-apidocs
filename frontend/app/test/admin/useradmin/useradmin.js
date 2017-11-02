var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var UserAdminView = require(
  "../../../scripts/admin/useradmin/useradmin"
);

var UserDetailsView = require(
  "../../../scripts/admin/useradmin/userdetails/userdetails"
).UserDetailsView;

var StatsView = require("../../../scripts/stats/stats");

var DocumentsList = require("../../../scripts/admin/documentslist");

describe("admin/useradmin/useradmin", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || UserAdminView;

    var actualProps = underscore.extendOwn(
      {
        userId: "1",
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
    assert.equal($("li:nth-child(2)", tabs).text(), "User details");
    assert.equal($("li:nth-child(3)", tabs).text(), "Statistics");
    assert.equal($("li:nth-child(4)", tabs).text(), "Documents");
  });

  it("should activate the details tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#details");
  });

  it("should activate the statistics tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
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

  it("should activate the documents tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
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
          component, UserDetailsView
        );

        assert.equal(detailsView.props.userId, "1");

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

        assert.equal(statsView.props.userId, "1");

        done();
      }
    );
  });

  it("should configure and render the documents view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
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
        assert.equal(documentsView.props.userid, "1");

        done();
      }
    );
  });
});
