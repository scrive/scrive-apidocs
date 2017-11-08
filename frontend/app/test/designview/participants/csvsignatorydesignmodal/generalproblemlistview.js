var React = require("react");
var underscore = require("underscore");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var CSVProblemModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvproblemmodel.jsx"
);
var GeneralProblemListView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/generalproblemlistview.jsx"
);

describe("designview/participants/csvsignatorydesignmodal/generalproblemlistview", function () {
  var container = null;
  var problems = null;

  beforeEach(function () {
    problems = [
      new CSVProblemModel({description: "spam"}),
      new CSVProblemModel({description: "eggs"}),
    ];
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
        problems: problems
      },
      props || {}
    );

    var component = React.render(
      React.createElement(GeneralProblemListView, actualProps), container
    );

    return component;
  };

  it("should render the header problem if it's specified", function () {
    var headerProblem = new CSVProblemModel({
      description: "header",
      header: true
    });
    var newProblems = [headerProblem].concat(problems);

    var component = renderComponent({problems: newProblems});

    var headerProblemViews = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "problem-header"
    );
    assert.lengthOf(headerProblemViews, 1);

    var headerProblemView = headerProblemViews[0];
    assert.equal(
      headerProblemView.getDOMNode().innerText, headerProblem.description()
    );
  });

  it("should render problems", function () {
    var component = renderComponent();

    var problemViews = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "problem"
    );
    assert.lengthOf(problemViews, 2);

    var problemView = problemViews[0];
    assert.equal(
      problemView.getDOMNode().innerText, problems[0].description()
    );
  });
});
