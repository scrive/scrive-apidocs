var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var ExtraDetailsView = require("../../../scripts/signview/extradetails/extradetailsview");
var Field = require("../../../js/fields.js").Field;

var TestUtils = React.addons.TestUtils;

xdescribe("signview/extradetails/extradetailsview", function () {
  var server, doc, model;

  var SignView = Backbone.Model.extend({
    askForName: function () {
      return true;
    },

    askForEmail: function () {
      return true;
    },

    askForSSN: function () {
      return false;
    },

    askForSSNIfNotEID: function () {
      return false;
    },

    askForPhone: function () {
      return false;
    },

    askForPhoneIfNotPin: function () {
      return false;
    }
  });

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        ExtraDetailsView,
        {model: doc.currentSignatory(), signview: model, isVertical: false}
      ),
      $('body')[0]
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (d) {
      doc = d;
      model = new SignView();

      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should initialize state from sign view model when it mounts", function () {
    var component = renderComponent();

    assert.isTrue(component.state.askForName);
    assert.isTrue(component.state.askForEmail);
    assert.isFalse(component.state.askForSSN);
    assert.isFalse(component.state.askForPhone);
  });

  it("should create a task from a ref", function () {
    var component = renderComponent();
    sinon.stub(component.refs.name, "focus");

    var isComplete = sinon.stub().returns(true)
    var nameTask = component.createTaskFromRef(component.refs.name, isComplete);

    assert.equal(nameTask.get("type"), "extra-details");
    assert.isTrue(nameTask.isComplete());
    assert.isTrue(isComplete.called);
    assert.equal(nameTask.get("el")[0], component.refs.name.getDOMNode());

    nameTask.onArrowClick();
    assert.isTrue(component.refs.name.focus.called);
  });

  it("should create a tasks", function () {
    var fakeSSNField = new Field({value: "ssn"});
    sinon.stub(doc.currentSignatory(), "personalnumberField").returns(fakeSSNField);

    sinon.stub(model, "askForSSNIfNotEID").returns(true);
    sinon.stub(model, "askForSSN").returns(true);
    sinon.stub(model, "askForPhoneIfNotPin").returns(true);
    sinon.stub(model, "askForPhone").returns(true);

    var component = renderComponent();

    tasks = component.createTasks();
    assert.lengthOf(tasks, 4);
  });

  it("should refocus a field", function () {
    var component = renderComponent();
    sinon.stub(component.refs.name, "focus");

    component.setState({focus: "name"});

    component.refocus();
    assert.isTrue(component.refs.name.focus.called);
  });

  it("should blur a field", function () {
    var component = renderComponent();
    component.setState({focus: "name"});

    component.handleBlur();
    assert.equal(component.state.focus, "");
  });

  it("should focus a field", function () {
    var component = renderComponent();
    component.setState({focus: "name"});

    var nameFocusHandler = component.handleFocus("name");
    nameFocusHandler();

    assert.equal(component.state.focus, "name");
  });

  it("should render the name field", function () {
    var component = renderComponent();

    var nameField = $("input#name");
    assert.lengthOf(nameField, 1);
    assert.equal(nameField.val(), doc.currentSignatory().name());
  });

  it("should render the e-mail field", function () {
    var component = renderComponent();

    var emailField = $("input#email");
    assert.lengthOf(emailField, 1);
    assert.equal(emailField.val(), doc.currentSignatory().emailField().value());
  });

  it("should render the SSN field", function () {
    var fakeSSNField = new Field({value: "ssn"});
    sinon.stub(doc.currentSignatory(), "personalnumberField").returns(fakeSSNField);

    sinon.stub(model, "askForSSNIfNotEID").returns(true);
    sinon.stub(model, "askForSSN").returns(true);

    var component = renderComponent();

    var ssnField = $("input#ssn");
    assert.lengthOf(ssnField, 1);
    assert.equal(ssnField.val(), fakeSSNField.value());
  });

  it("should render the Phone field", function () {
    sinon.stub(model, "askForPhoneIfNotPin").returns(true);
    sinon.stub(model, "askForPhone").returns(true);

    var component = renderComponent();

    var phoneField = $("input#phone");
    assert.lengthOf(phoneField, 1);
    assert.equal(phoneField.val(), doc.currentSignatory().mobileField().value());
  });
});
