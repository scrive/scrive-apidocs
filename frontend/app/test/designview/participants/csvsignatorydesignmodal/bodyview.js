var React = require("react");
var underscore = require("underscore");

var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var BodyView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/bodyview.jsx"
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
var GeneralProblemListView = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/generalproblemlistview.jsx"
);

describe("designview/participants/csvsignatorydesignmodal/bodyview", function () {
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
        model: model
      },
      props || {}
    );

    var component = React.render(
      React.createElement(BodyView, actualProps), container
    );

    return component;
  };

  it("should configure and render the general problem list view", function () {
    var generalProblems = [
      new CSVProblemModel({
        row: undefined,
        cell: undefined,
        description: "spam"
      })
    ];
    sinon.stub(model, "generalProblems").returns(generalProblems);

    var component = renderComponent();

    var generalProblemListView = TestUtils.findRenderedComponentWithType(
      component, GeneralProblemListView
    );
    assert.equal(generalProblemListView.props.problems, generalProblems);
  });

  it("should render table headers", function () {
    var component = renderComponent();

    var headers = TestUtils.scryRenderedDOMComponentsWithTag(
      component, "th"
    );
    assert.lengthOf(headers, 3);
    assert.equal(headers[0].getDOMNode().innerText, localization.fstname);
    assert.equal(headers[1].getDOMNode().innerText, localization.sndname);
    assert.equal(headers[2].getDOMNode().innerText, localization.email);
  });

  it("should configure and render table row views", function () {
    var component = renderComponent();

    var rowViews = TestUtils.scryRenderedComponentsWithType(
      component, CSVDataTableRowView
    );
    assert.lengthOf(rowViews, 1);

    var rowView = rowViews[0];
    assert.equal(rowView.props.index, 0);
    assert.equal(rowView.props.model, model);
    assert.equal(rowView.props.row, model.rows()[0]);
  });
});