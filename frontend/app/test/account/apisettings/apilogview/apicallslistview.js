var moment = require("moment");
var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var APICallModel = require(
  "../../../../scripts/account/apisettings/apilogview/apicallmodel"
);
var APICallsCollection = require(
  "../../../../scripts/account/apisettings/apilogview/apicallscollection"
);
var APICallsListItemView = require(
  "../../../../scripts/account/apisettings/apilogview/apicallslistitemview"
);
var APICallsListView = require(
  "../../../../scripts/account/apisettings/apilogview/apicallslistview"
);

describe("account/apisettings/apilogview/apicallslistview", function () {
  var container = null;
  var calls = null;

  beforeEach(function () {
    calls = new APICallsCollection([
      new APICallModel({
        id: 1,
        time: moment(),
        responseBody: {spam: true},
        responseCode: 200,
        requestMethod: "GET",
        requestParamsGET: [],
        requestParamsPOST: [],
        requestURI: "/apm/spam"
      }),
      new APICallModel({
        id: 2,
        time: moment(),
        responseBody: {spam: true},
        responseCode: 200,
        requestMethod: "GET",
        requestParamsGET: [],
        requestParamsPOST: [],
        requestURI: "/apm/spam"
      })
    ]);
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        calls: calls,
        hidden: false,
        onSelect: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(APICallsListView, actualProps), container
    );

    return component;
  };

  it("should render as hidden if it's hidden", function () {
    var component = renderComponent({hidden: true});
    assert.isTrue(component.getDOMNode().classList.contains("hidden"));
  });

  it("should not render as hidden if isn't hidden", function () {
    var component = renderComponent({hidden: false});
    assert.isFalse(component.getDOMNode().classList.contains("hidden"));
  });

  it("should configure and render the list item views", function () {
    var component = renderComponent();

    var items = TestUtils.scryRenderedComponentsWithType(
      component, APICallsListItemView
    );
    assert.lengthOf(items, 2);

    var item = items[0];
    assert.equal(item.props.call, calls.at(0));
    assert.equal(item.props.onSelect, component.props.onSelect);
  });
});
