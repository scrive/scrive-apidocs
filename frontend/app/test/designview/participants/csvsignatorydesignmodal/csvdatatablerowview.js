var React = require("react");
var underscore = require("underscore");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var CSVDataTableCellView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvdatatablecellview.jsx"
);
var CSVDataTableRowView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvdatatablerowview.jsx"
);
var CSVProblemModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvproblemmodel.jsx"
);
var CSVSignatoryDesignModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvsignatorydesignmodel.jsx"
);

describe("designview/participants/csvsignatorydesignmodal/csvdatatablerowview", function () {
  var container = null;
  var model = null;

  beforeEach(function () {
    model = new CSVSignatoryDesignModel({
      header: ["fstname", "sndname", "email"],
      problems: [],
      rows: [
        ["Spam", "Eggs", "spam@eggs.com"]
      ]
    })
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
        index: 0,
        model: model,
        row: model.rows()[0]
      },
      props || {}
    );

    var component = React.render(
      React.createElement(CSVDataTableRowView, actualProps), container
    );

    return component;
  };

  it("should configure and render the cell views", function () {
    var problem = new CSVProblemModel();
    sinon.stub(model, "problemWithCell").returns(problem);

    var component = renderComponent();

    var cellViews = TestUtils.scryRenderedComponentsWithType(
      component, CSVDataTableCellView
    );
    assert.lengthOf(cellViews, 3);

    var cellView = cellViews[0];
    assert.equal(cellView.props.cell, model.rows()[0][0]);
    assert.isTrue(model.problemWithCell.calledWith(0, 0));
    assert.equal(cellView.props.problem, problem);
  });

  it("should not render the problem view if there's no problem for the row", function () {
    sinon.stub(model, "problemWithRow").returns(undefined);

    var component = renderComponent();

    var problemViews = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "row-problem"
    );
    assert.lengthOf(problemViews, 0);
  });

  it("should render the problem view if there's a problem for the row", function () {
    var problem = new CSVProblemModel();
    sinon.stub(model, "problemWithRow").returns(problem);

    var component = renderComponent();

    var problemView = TestUtils.findRenderedDOMComponentWithClass(
      component, "problem-row"
    );
    assert.equal(problemView.getDOMNode().innerText, problem.description());
  });
});
