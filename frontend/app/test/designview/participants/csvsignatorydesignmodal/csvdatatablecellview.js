var React = require("react");
var underscore = require("underscore");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var CSVDataTableCellView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvdatatablecellview.jsx"
);
var CSVProblemModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvproblemmodel.jsx"
);

describe("designview/participants/csvsignatorydesignmodal/csvdatatablecellview", function () {
  var container = null;

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
        cell: "spam"
      },
      props || {}
    );

    var component = React.render(
      React.createElement(CSVDataTableCellView, actualProps), container
    );

    return component;
  };

  it("should render the cell content", function () {
    var component = renderComponent();

    var content = TestUtils.findRenderedDOMComponentWithClass(
      component, "content"
    );
    assert.equal(content.getDOMNode().innerText, "spam");
  });

  it("should not render the problem view if the problem is undefined", function () {
    var component = renderComponent();

    var problemViews = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "problem"
    );
    assert.lengthOf(problemViews, 0);
  });

  it("should render the problem view if the problem is specified", function () {
    var problem = new CSVProblemModel();
    var component = renderComponent({problem: problem});

    var problemView = TestUtils.findRenderedDOMComponentWithClass(
      component, "problem"
    );
    assert.equal(problemView.getDOMNode().innerText, problem.description());
  });
});
