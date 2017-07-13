var Backbone = require("backbone");
var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;

var backend = require("../../backend");
var util = require("../../util");

var Field = require("../../../js/fields.js").Field;
var RadioButton = require("../../../scripts/icons/radiobutton.jsx");
var RadioButtonPlacementPlacedView = require(
  "../../../scripts/signview/fileview/radiobuttonplacementplacedview.jsx"
);
var Task = require("../../../scripts/signview/navigation/task.jsx");
var TaskList = require("../../../scripts/signview/navigation/task_list.jsx");

var FakeSignview = Backbone.Model.extend({});

var WrapperComponent = React.createClass({
  childContextTypes: {
    taskList: React.PropTypes.instanceOf(TaskList).isRequired
  },
  getChildContext: function () {
    return {
      taskList: this.props.taskList
    };
  },
  render: function () {
    return React.createElement(
      RadioButtonPlacementPlacedView,
      {
        ref: "placementView",
        model: this.props.model,
        pageWidth: this.props.pageWidth,
        pageHeight: this.props.pageHeight,
        signview: this.props.signview
      }
    );
  }
});

describe("signview/fileview/radiobuttonplacementplacedview", function () {
  var container = null;
  var document_ = null;
  var placement = null;
  var taskList = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        model: placement,
        pageWidth: 100,
        pageHeight: 100,
        signview: new FakeSignview(),
        taskList: taskList
      },
      props || {}
    );

    var component = React.render(
      React.createElement(WrapperComponent, actualProps),
      container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      var field = new Field({
        signatory: document_.signatoriesWhoSign()[0],
        name: "Radio Group",
        radio_button_values: [
          "Radio Button 1", "Radio Button 2", "Radio Button 3"
        ],
        placements: [
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.1,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.2,
            yrel: 0.2
          },
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.3,
            yrel: 0.3
          }
        ]
      });

      placement = field.placements()[0];

      taskList = new TaskList();

      done();
    });
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    server.restore();
  });

  it("should not be active if signatory isn't the current signatory", function () {
    sinon.stub(document_, "currentSignatory").returns(
      document_.signatories()[1]
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);

    var component = renderComponent();
    assert.isFalse(component.refs.placementView.isActive());
  });

  it("should not be active if the current signatory can't sign", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);

    var component = renderComponent();
    assert.isFalse(component.refs.placementView.isActive());
  });

  it("should be active", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);

    var component = renderComponent();
    assert.isTrue(component.refs.placementView.isActive());
  });

  it("should not be selected if the field isn't ready to sign", function () {
    sinon.stub(placement.field(), "readyForSign").returns(false);

    var component = renderComponent();
    assert.isFalse(component.refs.placementView.hasSelection());
  });

  it("should be selected", function () {
    sinon.stub(placement.field(), "readyForSign").returns(true);

    var component = renderComponent();
    assert.isTrue(component.refs.placementView.hasSelection());
  });

  it("should not create the task if the signatory isn't current", function () {
    sinon.stub(placement.field().signatory(), "current").returns(false);

    var component = renderComponent();
    assert.isUndefined(component.refs.placementView.createTasks());
  });

  it("should create the task", function () {
    var component = renderComponent();

    var tasks = component.refs.placementView.createTasks();
    assert.isArray(tasks);
    assert.equal(tasks.length, 1);
    assert.instanceOf(tasks[0], Task);
  });

  it("should cancel the propagation of onMouseDown event", function () {
    var fakeEvent = {
      preventDefault: sinon.stub(),
      stopPropagation: sinon.stub()
    };

    var component = renderComponent();

    component.refs.placementView.onMouseDown(fakeEvent);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isTrue(fakeEvent.stopPropagation.called);
  });

  it("should not set the placement as selected if it isn't active", function () {
    sinon.stub(placement.field(), "setSelectedRadioButton");

    var component = renderComponent();
    sinon.stub(component.refs.placementView, "isActive").returns(false);

    TestUtils.Simulate.click(component.refs.placementView.getDOMNode());
    assert.isFalse(placement.field().setSelectedRadioButton.called);
  });

  it("should not set the placement as selected when it's clicked", function () {
    sinon.stub(placement.field(), "setSelectedRadioButton");

    var component = renderComponent();
    sinon.stub(component.refs.placementView, "isActive").returns(true);

    TestUtils.Simulate.click(component.refs.placementView.getDOMNode());
    assert.isTrue(placement.field().setSelectedRadioButton.calledWith(
      placement
    ));
  });

  it("should not render the clickable area when it's inactive", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);

    var component = renderComponent();

    var elements = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "clickable"
    );

    assert.equal(elements.length, 0);
  });

  it("should render the clickable area", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);

    var component = renderComponent();

    var elements = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "clickable"
    );

    assert.equal(elements.length, 1);
  });

  it("should not render the underlay when it's inactive", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);
    sinon.stub(placement.field(), "readyForSign").returns(false);

    var component = renderComponent();

    var elements = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "underlay"
    );

    assert.equal(elements.length, 0);
  });

  it("should not render the underlay when the field is ready to sign", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);
    sinon.stub(placement.field(), "readyForSign").returns(true);

    var component = renderComponent();

    var elements = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "underlay"
    );

    assert.equal(elements.length, 0);
  });

  it("should render the underlay", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);
    sinon.stub(placement.field(), "readyForSign").returns(false);

    var component = renderComponent();

    var elements = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "underlay"
    );

    assert.equal(elements.length, 1);
  });

  it("should render the radio button", function () {
    sinon.stub(document_, "currentSignatory").returns(
      placement.field().signatory()
    );
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);
    sinon.stub(placement.field(), "readyForSign").returns(false);

    var component = renderComponent();

    var radioButton = TestUtils.findRenderedComponentWithType(
      component, RadioButton
    );

    assert.isTrue(radioButton.props.active);
    assert.equal(
      radioButton.props.pageWidth, component.refs.placementView.props.pageWidth
    );
    assert.isFalse(radioButton.props.selected);
  });
});
