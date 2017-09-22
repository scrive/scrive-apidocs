var React = require("react");
var underscore = require("underscore");

var backend = require("../../backend");
var TestUtils = React.addons.TestUtils;
var util = require("../../util");

var ParticipantMobileField = require(
  "../../../scripts/designview/participants/participantmobilefield"
);
var PhoneNumberInput = require(
  "../../../scripts/common/phone_number_input"
);
var Track = require("../../../scripts/common/track");

describe("scripts/designview/participants/participantmobilefield", function () {
  var container = null;
  var field = null;
  var server = null;

  before(function () {
    server = backend.createServer();
  })

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      field = document_.signatoriesWhoSign()[0].mobileField();

      sinon.stub(Track, "track");

      done();
    });
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    Track.track.restore();

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    server.restore();
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        last: false,
        model: field
      },
      props || {}
    );

    var component = React.render(
      React.createElement(ParticipantMobileField, actualProps),
      container
    );

    return component;
  };

  it("should return the placeholder text", function () {
    var component = renderComponent();

    var result = component.placeholderText();
    assert.equal(result, localization.phonePlaceholder);
  });

  it("should update model when input value changes", function () {
    sinon.stub(field, "setValue");

    var component = renderComponent();

    component.onInputChange("spam");
    assert.isTrue(field.setValue.calledWith("spam"));
  });

  describe("onInputTab", function () {
    var event = null;

    beforeEach(function () {
      event = {};
      event.preventDefault = sinon.stub();
    });

    it("should cancel the event if it's the last field", function () {
      var component = renderComponent({last: true});

      component.onInputTab(event);
      assert.isTrue(event.preventDefault.called);
    });

    it("should not cancel the event if it isn't the last field", function () {
      var component = renderComponent({last: true});

      component.onInputTab(event);
      assert.isTrue(event.preventDefault.called);
    });
  });

  it("should remove the field", function () {
    sinon.stub(field, "removeAllPlacements");
    sinon.stub(field.signatory(), "deleteField");

    var component = renderComponent();

    component.remove();
    assert.isTrue(Track.track.called);
    assert.isTrue(field.removeAllPlacements.called);
    assert.isTrue(field.signatory().deleteField.calledWith(field));
  });

  describe("onInputRemove", function () {
    it("should not remove the field if it can't be removed", function () {
      sinon.stub(field, "canBeRemoved").returns(false);

      var component = renderComponent();
      sinon.stub(component, "remove");

      component.onInputRemove();
      assert.isFalse(component.remove.called);
    });

    it("should remove the field", function () {
      sinon.stub(field, "canBeRemoved").returns(true);

      var component = renderComponent();
      sinon.stub(component, "remove");

      component.onInputRemove();
      assert.isTrue(component.remove.called);
    });
  });

  describe("render", function () {
    it("should configure and render the input", function () {
      var component = renderComponent();

      var input = TestUtils.findRenderedComponentWithType(
        component, PhoneNumberInput.PhoneNumberInput
      );
      assert.match(input.props.className, /s-input-mobile/);
      assert.equal(input.props.infotext, component.placeholderText());
      assert.isFalse(input.props.readonly);
      assert.isFalse(input.props.disabled);
      assert.equal(input.props.value, field.value());
      assert.equal(input.props.onChange, component.onInputChange);
      assert.equal(input.props.onRemove, component.onInputRemove);
      assert.equal(input.props.onTab, component.onInputTab);
    });

    it("should configure and render the input from CSV", function () {
      sinon.stub(field, "isCsvField").returns(true);
      sinon.stub(field, "isValid").returns(true);

      var component = renderComponent();

      var input = TestUtils.findRenderedComponentWithType(
        component, PhoneNumberInput.PhoneNumberInput
      );
      assert.match(input.props.className, /transparent/);
      assert.equal(input.props.infotext, (
        component.placeholderText() + " (" + localization.designview.fromCSV +
        ")"
      ));
      assert.isTrue(input.props.readonly);
      assert.isTrue(input.props.disabled);
    });

    it("should configure and render readonly input if the author can't change it", function () {
      sinon.stub(field, "isAuthorUnchangeableField").returns(true);

      var component = renderComponent();

      var input = TestUtils.findRenderedComponentWithType(
        component, PhoneNumberInput.PhoneNumberInput
      );
      assert.match(input.props.className, /transparent/);
      assert.isTrue(input.props.readonly);
      assert.isTrue(input.props.disabled);
    });

    it("should configure and render readonly input if it's invalid", function () {
      sinon.stub(field, "isValid").returns(false);

      var component = renderComponent();

      var input = TestUtils.findRenderedComponentWithType(
        component, PhoneNumberInput.PhoneNumberInput
      );
      assert.match(input.props.className, /redborder/);
    });
  });
});
