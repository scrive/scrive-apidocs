var moment = require("moment");
var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var APICallModel = require(
  "../../../../scripts/account/apisettings/apilogview/apicallmodel"
);
var APICallsListItemView = require(
  "../../../../scripts/account/apisettings/apilogview/apicallslistitemview"
);

describe("account/apisettings/apilogview/apicallslistitemview", function () {
  var container = null;
  var call = null;

  beforeEach(function () {
    call = new APICallModel({
      id: 1,
      time: moment(),
      responseBody: {spam: true},
      responseCode: 200,
      requestMethod: "GET",
      requestParamsGET: [],
      requestParamsPOST: [],
      requestURI: "/apm/spam"
    });
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  var renderComponent = function (props) {
    var table = document.createElement("table");

    container = document.createElement("tbody");
    table.appendChild(container);

    var actualProps = underscore.extendOwn(
      {
        call: call,
        onSelect: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(APICallsListItemView, actualProps), container
    );

    return component;
  };

  it("should call the onSelect callback when it's selected or deselected", function () {
    var component = renderComponent();

    component.onSelect();
    assert.isTrue(component.props.onSelect.calledWith(call));
  });

  describe("render", function () {
    it("should set the onClick handler on the root element", function () {
      var component = renderComponent();

      var row = TestUtils.findRenderedDOMComponentWithTag(component, "tr");
      assert.equal(row.props.onClick, component.onSelect);
    });

    it("should render the call time cell", function () {
      sinon.stub(call, "displayTime").returns("spam");

      var component = renderComponent();

      var cells = TestUtils.scryRenderedDOMComponentsWithTag(component, "td");

      var timeCell = cells[0];
      assert.equal(timeCell.getDOMNode().innerText, "spam");
    });

    it("should render the call URI cell", function () {
      var component = renderComponent();

      var cells = TestUtils.scryRenderedDOMComponentsWithTag(component, "td");

      var uriCell = cells[1];
      assert.equal(uriCell.getDOMNode().innerText, call.get("requestURI"));
    });

    it("should render the call response status code cell as positive is the request was succesful", function () {
      sinon.stub(call, "isSuccessful").returns(true);

      var component = renderComponent();

      var requestStatusCell = TestUtils.findRenderedDOMComponentWithClass(
        component, "request-status"
      );

      assert.isTrue(
        requestStatusCell.getDOMNode().classList.contains("text-positivecolor")
      );
    });

    it("should render the call response status code cell as negative is the request wasn't succesful", function () {
      sinon.stub(call, "isSuccessful").returns(false);

      var component = renderComponent();

      var requestStatusCell = TestUtils.findRenderedDOMComponentWithClass(
        component, "request-status"
      );

      assert.isTrue(
        requestStatusCell.getDOMNode().classList.contains("text-negativecolor")
      );
    });

    it("should render the call response status code ", function () {
      var component = renderComponent();

      var requestStatusCell = TestUtils.findRenderedDOMComponentWithClass(
        component, "request-status"
      );

      assert.equal(
        requestStatusCell.getDOMNode().innerText, call.get("responseCode")
      );
    });
  });
});
