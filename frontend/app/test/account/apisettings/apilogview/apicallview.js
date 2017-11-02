var moment = require("moment");
var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var APICallModel = require(
  "../../../../scripts/account/apisettings/apilogview/apicallmodel"
);
var APICallView = require(
  "../../../../scripts/account/apisettings/apilogview/apicallview"
);

describe("account/apisettings/apilogview/apicallview", function () {
  var container = null;

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  describe("ParametersSectionView", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          header: "spam",
          params: [["spam", "eggs"], ["eggs", "spam"]],
          placeholder: "eggs"
        },
        props || {}
      );

      var component = React.render(
        React.createElement(APICallView.ParametersSectionView, actualProps),
        container
      );

      return component;
    };

    it("should render the header", function () {
      var component = renderComponent();

      var $header = $(".header", component.getDOMNode());
      assert.equal($header.text(), component.props.header);
    });

    it("should render the placholder if params array is empty", function () {
      var component = renderComponent({params: []});
      assert.lengthOf($("table", component.getDOMNode()), 0);

      var $placeholder = $(".placeholder", component.getDOMNode());
      assert.equal($placeholder.text(), component.props.placeholder);
    });

    it("should render the params table", function () {
      var component = renderComponent();
      assert.lengthOf($(".placeholder", component.getDOMNode()), 0);
      assert.lengthOf($("table tbody > tr", component.getDOMNode()), 2);

      var row = $("table tbody > tr:first-child", component.getDOMNode());
      assert($("td:nth-child(1)", row).text(), component.props.params[0][0]);
      assert($("td:nth-child(2)", row).text(), component.props.params[0][1]);
    });
  });

  describe("BodySectionView", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          body: "body",
          header: "spam",
          placeholder: "eggs"
        },
        props || {}
      );

      var component = React.render(
        React.createElement(APICallView.BodySectionView, actualProps),
        container
      );

      return component;
    };

    it("should initialize state", function () {
      var component = renderComponent();
      assert.isTrue(component.state.collapsed);
    });

    it("should toggle the collapsed state when the expander is clicked", function () {
      var component = renderComponent();

      component.onCollapserExpanderClick();
      assert.isFalse(component.state.collapsed);
    });

    it("should render the header", function () {
      var component = renderComponent();

      var $header = $(".header", component.getDOMNode());
      assert.equal($header.text(), component.props.header);
    });

    it("should render the placholder if body is empty", function () {
      var component = renderComponent({body: undefined});
      assert.lengthOf($(".pre-wrapper", component.getDOMNode()), 0);

      var $placeholder = $(".placeholder", component.getDOMNode());
      assert.equal($placeholder.text(), component.props.placeholder);
    });

    it("should render the body view as collapsed if it's collapsed", function () {
      var component = renderComponent();

      var $preWrapper = $(".pre-wrapper", component.getDOMNode())
      assert.isTrue($preWrapper.hasClass("collapsed"));
    });

    it("should not render the body view as collapsed if it's expanded", function () {
      var component = renderComponent();
      component.setState({collapsed: false});

      var $preWrapper = $(".pre-wrapper", component.getDOMNode())
      assert.isFalse($preWrapper.hasClass("collapsed"));
    });

    it("should render the body", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.bodyView);
      assert.equal(component.refs.bodyView.getDOMNode().innerText, "\"body\"");
    });

    it("should not render the expander if it's expanded", function () {
      var component = renderComponent();
      component.setState({collapsed: false});

      assert.isUndefined(component.refs.expander);
    });

    it("should configure and render the expander if it's collapsed", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.expander);
      assert.equal(
        component.refs.expander.props.onClick,
        component.onCollapserExpanderClick
      );
    });

    it("should not render the collapser if it's collapsed", function () {
      var component = renderComponent();
      assert.isUndefined(component.refs.collapser);
    });

    it("should configure and render the collapser if it's expanded", function () {
      var component = renderComponent();
      component.setState({collapsed: false});

      assert.isDefined(component.refs.collapser);
      assert.equal(
        component.refs.collapser.props.onClick,
        component.onCollapserExpanderClick
      );
    });
  });

  describe("APICallView", function () {
    var call = null;

    beforeEach(function () {
      call = new APICallModel({
        id: 1,
        time: moment(),
        responseBody: {spam: true},
        responseCode: 200,
        requestMethod: "GET",
        requestParamsGET: [["spam", "eggs"]],
        requestParamsPOST: [["eggs", "spam"]],
        requestURI: "/apm/spam"
      });
    });

    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          call: call,
          onClose: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(APICallView.APICallView, actualProps),
        container
      );

      return component;
    };

    it("should render the request method and URI", function () {
      var component = renderComponent();

      var $uriView = $(".meta .uri", component.getDOMNode());
      assert.equal($uriView.text(), "GET /apm/spam");
    });

    it("should render the request time", function () {
      sinon.stub(call, "displayTime").returns("spam");

      var component = renderComponent();

      var $timeView = $(".meta .time", component.getDOMNode());
      assert.equal($timeView.text(), "spam");
    });

    it("should render the request time", function () {
      sinon.stub(call, "displayTime").returns("spam");

      var component = renderComponent();

      var $timeView = $(".meta .time", component.getDOMNode());
      assert.equal($timeView.text(), "spam");
    });

    it("should render the response status code as positive if the request was successful", function () {
      sinon.stub(call, "isSuccessful").returns(true);

      var component = renderComponent();

      var $responseCodeView = $(
        ".meta .response-code", component.getDOMNode()
      );
      assert.isTrue($responseCodeView.hasClass("text-positivecolor"));
    });

    it("should render the response status code as positive if the request wasn't successful", function () {
      sinon.stub(call, "isSuccessful").returns(false);

      var component = renderComponent();

      var $responseCodeView = $(
        ".meta .response-code", component.getDOMNode()
      );
      assert.isTrue($responseCodeView.hasClass("text-negativecolor"));
    });

    it("should configure and render the parameters section views", function () {
      var component = renderComponent();

      var parametersSectionViews = TestUtils.scryRenderedComponentsWithType(
        component, APICallView.ParametersSectionView
      );
      assert.lengthOf(parametersSectionViews, 2);

      var getParametersSectionView = parametersSectionViews[0];
      assert.equal(
        getParametersSectionView.props.params, call.get("requestParamsGET")
      );

      var postParametersSectionView = parametersSectionViews[1];
      assert.equal(
        postParametersSectionView.props.params, call.get("requestParamsPOST")
      );
    });

    it("should configure and render the body section views", function () {
      var component = renderComponent();

      var bodySectionViews = TestUtils.scryRenderedComponentsWithType(
        component, APICallView.BodySectionView
      );
      assert.lengthOf(bodySectionViews, 1);

      var responseBodySectionView = bodySectionViews[0];
      assert.equal(
        responseBodySectionView.props.body, call.get("responseBody")
      );
    });

    it("should configure and render the closer", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.closer);
      assert.equal(
        component.refs.closer.props.onClick, component.props.onClose
      );
    });
  });
});
