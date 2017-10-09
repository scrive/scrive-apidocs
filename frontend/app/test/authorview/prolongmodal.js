var React = require("react");
var underscore = require("underscore");

var util = require("../util");

var Calendar = require("../../js/calendar.js");
var ProlongModal = require("../../scripts/authorview/prolongmodal.jsx");

describe("authorview/prolongmodal", function () {
  var container = null;
  var fakeCalendar = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        active: false,
        onAccept: sinon.stub(),
        onClose: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(ProlongModal, actualProps), container
    );

    return component;
  };

  beforeEach(function () {
    fakeCalendar = {
      close: sinon.stub(),
      setDays: sinon.stub()
    };

    sinon.stub(Calendar, "Calendar").returns(fakeCalendar);
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    Calendar.Calendar.restore();

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isTrue(component.state.acceptVisible);
    assert.equal(component.state.days, "1");
  });

  it("should create calendar control when it mounts", function () {
    var component = renderComponent();
    assert.isTrue(Calendar.Calendar.calledWithNew());

    var calendarOptions = Calendar.Calendar.getCall(0).args[0];
    assert.equal(calendarOptions.on[0], component.refs.calendarButton.getDOMNode());
    assert.equal(calendarOptions.days, component.state.days);
    assert.equal(calendarOptions.change, component.onCalendarChange);
  });

  describe("componentDidUpdate", function () {
    it("should update the calendar control when days value changes", function () {
      var component = renderComponent();

      component.setState({days: "2"});
      assert.isTrue(fakeCalendar.setDays.calledWith(2));
    });

    it("should hide the accept button if new days value isn't a number", function () {
      var component = renderComponent();

      component.setState({days: "spam"});
      assert.isFalse(component.state.acceptVisible);
    });

    it("should show the accept button if new days value is a number", function () {
      var component = renderComponent();

      component.setState({days: "2", acceptVisible: false});
      assert.isTrue(component.state.acceptVisible);
    });
  });

  it("should handle the modal being accepted", function () {
    var component = renderComponent();

    component.onAcceptButtonClick();
    assert.isTrue(fakeCalendar.close.called);
    assert.isTrue(component.props.onAccept.calledWith("1"));
  });

  it("should handle the modal being closed", function () {
    var component = renderComponent();

    component.onCloseButtonClick();
    assert.isTrue(fakeCalendar.close.called);
    assert.isTrue(component.props.onClose.called);
  });

  it("should clear state when it hides", function () {
    var component = renderComponent();
    component.setState({
      acceptVisible: false,
      days: "2"
    });

    component.onHide();
    assert.isTrue(component.state.acceptVisible);
    assert.equal(component.state.days, "1");
  });

  it("should update state when calendar control changes", function () {
    var component = renderComponent();

    component.onCalendarChange(2);
    assert.equal(component.state.days, "2");
  });

  it("should update state when days input changes", function () {
    var component = renderComponent();

    component.onDaysInputChange("2");
    assert.equal(component.state.days, "2");
  });

  describe("render", function () {
    it("should configure and render the days input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.daysInput);

      var input = component.refs.daysInput;
      assert.equal(input.props.infotext, "1");
      assert.equal(input.props.value, component.state.days);
      assert.equal(input.props.onChange, component.onDaysInputChange);
    });

    it("should render the calendar button", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.calendarButton);
    });

    it("should configure and render the accept button if it's visible", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.acceptModalButton);

      var button = component.refs.acceptModalButton;
      assert.equal(button.props.onClick, component.onAcceptButtonClick);
    });

    it("should not render the accept button if isn't visible", function () {
      var component = renderComponent();

      component.setState({acceptVisible: false});
      assert.isUndefined(component.refs.acceptModalButton);
    });
  });
});
