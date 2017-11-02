var moment = require("moment");
var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../../backend");
var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var APICallsCollection = require(
  "../../../../scripts/account/apisettings/apilogview/apicallscollection"
);
var APICallsListView = require(
  "../../../../scripts/account/apisettings/apilogview/apicallslistview"
);
var APICallView = require(
  "../../../../scripts/account/apisettings/apilogview/apicallview"
);
var APILogView = require("../../../../scripts/account/apisettings/apilogview");

describe("account/apisettings/apilogview", function () {
  var container = null;

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  beforeEach(function () {
    sinon.stub(window, "addEventListener");
    sinon.stub(window, "removeEventListener");
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    window.addEventListener.restore();
    window.removeEventListener.restore();

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    window.location.hash = "";
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
      React.createElement(APILogView, actualProps), container
    );

    return component;
  };

  it("should initialize state", function () {
    var component = renderComponent({loadLater: true});
    assert.isFalse(component.state.loading);
    assert.isNull(component.state.selectedCall);
  });

  describe("componentWillMount", function () {
    it("should create the collection instance", function () {
      var component = renderComponent();
      assert.instanceOf(component.collection, APICallsCollection);
    });
  });

  describe("componentDidMount", function () {
    it("should install the hashchange event handler", function () {
      var component = renderComponent();
      assert.isTrue(window.addEventListener.calledWith(
        "hashchange", component.onWindowHashChange
      ));
    });
  });

  describe("componentWillUnmount", function () {
    it("should uninstall the hashchange event handler", function () {
      var component = renderComponent();
      React.unmountComponentAtNode(container);
      container = null;

      assert.isTrue(window.removeEventListener.calledWith(
        "hashchange", component.onWindowHashChange
      ));
    });
  });

  it("it should reload data from the server", function () {
    var component = renderComponent({loadLater: true});
    sinon.stub(component.collection, "fetch");

    component.reload();
    assert.isTrue(component.state.loading);
    assert.isTrue(component.collection.fetch.calledWith({
      reset: true,
      success: component.onCollectionFetch
    }));
  });

  it("should handle collection fetch success", function () {
    var component = renderComponent({loadLater: true});
    component.setState({loading: true});

    component.onCollectionFetch();
    assert.isFalse(component.state.loading);
    assert.isNull(component.state.selectedCall);
  });

  it("should handle a call being selected", function (done) {
    var component = renderComponent({loadLater: true});

    component.reload();
    util.waitUntil(
      function () {
        return (component.state.loading === false);
      },
      function () {
        var selectedCall = component.collection.at(0);

        component.onCallListSelect(selectedCall);
        assert.equal(component.state.selectedCall, selectedCall);

        done();
      }
    );
  });

  it("should handle the call view being closed", function (done) {
    var component = renderComponent({loadLater: true});

    component.reload();
    util.waitUntil(
      function () {
        return (component.state.loading === false);
      },
      function () {
        component.setState({selectedCall: component.collection.at(0)});

        component.onCallViewClose();
        assert.isNull(component.state.selectedCall);

        done();
      }
    );
  });

  describe("render", function () {
    it("should configure and render the call list view", function (done) {
      var component = renderComponent({loadLater: true});

      component.reload();
      util.waitUntil(
        function () {
          return (component.state.loading === false);
        },
        function () {
          var listView = TestUtils.findRenderedComponentWithType(
            component, APICallsListView
          );
          assert.equal(listView.props.calls, component.collection);
          assert.isFalse(listView.props.hidden);
          assert.equal(listView.props.onSelect, component.onCallListSelect);

          done();
        }
      );
    });

    it("should render the list view as hidden when a call is selected", function (done) {
      var component = renderComponent({loadLater: true});

      component.reload();
      util.waitUntil(
        function () {
          return (component.state.loading === false);
        },
        function () {
          component.setState({selectedCall: component.collection.at(0)});

          var listView = TestUtils.findRenderedComponentWithType(
            component, APICallsListView
          );
          assert.isTrue(listView.props.hidden);

          done();
        }
      );
    });

    it("should not render the call view when no call is selected", function (done) {
      var component = renderComponent({loadLater: true});

      component.reload();
      util.waitUntil(
        function () {
          return (component.state.loading === false);
        },
        function () {
          var callViews = TestUtils.scryRenderedComponentsWithType(
            component, APICallView.APICallView
          );
          assert.lengthOf(callViews, 0);

          done();
        }
      );
    });

    it("should configure and render the call view when a call is selected", function (done) {
      var component = renderComponent({loadLater: true});

      component.reload();
      util.waitUntil(
        function () {
          return (component.state.loading === false);
        },
        function () {
          var selectedCall = component.collection.at(0);
          component.setState({selectedCall: selectedCall});

          var callView = TestUtils.findRenderedComponentWithType(
            component, APICallView.APICallView
          );
          assert.equal(callView.props.call, selectedCall);
          assert.equal(callView.props.onClose, component.onCallViewClose);

          done();
        }
      );
    });
  });
});
