var Backbone = require("backbone");
var React = require("react");
var $ = require("jquery");
var underscore = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var SignFinishView = require(
  "../../../scripts/signview/signsection/signfinishview"
);
var SignViewModel = require("../../../scripts/signview/signviewmodel");
var Task = require("../../../scripts/signview/navigation/task");

var FakeSignViewModel = Backbone.Model.extend({
  defaults: {
    document: null
  },
  document: function () {
    return this.get("document");
  },
  hasRejectOption: function () {
    return true;
  },
  hasForwardOption: function () {
    return false;
  }
});

var WrapperComponent = React.createClass({
  displayName: "WrapperComponent",
  childContextTypes: {
    blinkArrow: React.PropTypes.func
  },
  getChildContext: function () {
    return {
      blinkArrow: sinon.stub()
    }
  },
  render: function () {
    return React.createElement(
      SignFinishView,
      {
        ref: "signFinishView",
        model: this.props.model,
        title: this.props.title,
        name: this.props.name,
        canSign: this.props.canSign,
        onSign: this.props.onSign,
        onReject: this.props.onReject
      }
    )
  }
});

describe("signview/signsection/signfinishview", function () {
  var container = null;
  var server = null;
  var document_ = null;
  var model = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        model: model,
        title: model.document().title(),
        name: model.document().currentSignatory().name(),
        canSign: true,
        onSign: sinon.stub(),
        onReject: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(WrapperComponent, actualProps), container
    );

    return component.refs.signFinishView;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      model = new SignViewModel({document: document_, siglinkid: 1});

      done();
    });
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
  });

  it("should create the task", function () {
    var component = renderComponent();

    var tasks = component.createTasks();
    assert.isArray(tasks);
    assert.equal(tasks.length, 1);

    var task = tasks[0];
    assert.instanceOf(task, Task);
    assert.isTrue(task.isSignTask());
  });

  it("should trigger onSign callback when the sign button is clicked", function () {
    var component = renderComponent();

    TestUtils.Simulate.click(component.refs.signButton.getDOMNode());

    assert.isTrue(component.props.onSign.called);
    assert.isFalse(component.context.blinkArrow.called);
  });

  it("should blink arrow when the sign button is clicked and document can't be signed", function () {
    var component = renderComponent({canSign: false});

    TestUtils.Simulate.click(component.refs.signButton.getDOMNode());

    assert.isFalse(component.props.onSign.called);
    assert.isTrue(component.context.blinkArrow.called);
  });

  it("should trigger onReject callback when the reject buttson is clicked", function () {
    var component = renderComponent();

    TestUtils.Simulate.click(component.refs.rejectButton.getDOMNode());

    assert.isTrue(component.props.onReject.called);
  });

  it("should render inactive sign button if the document can't be signed", function () {
    var component = renderComponent({canSign: false});
    var signButton = component.refs.signButton;

    assert.notEqual(signButton.props.className.indexOf("inactive"), -1);
  });

  it("should not render reject button if the document can't be rejected", function () {
    sinon.stub(model, "hasRejectOption").returns(false);
    var component = renderComponent();

    assert.isUndefined(component.refs.rejectButton);
  });
});
