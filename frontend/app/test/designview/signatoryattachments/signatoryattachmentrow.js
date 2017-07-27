var React = require("react");
var _ = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var DesignSignatoryAttachment = require(
  "../../../scripts/designview/signatoryattachments/designsignatoryattachment"
);
var SignatoryAttachmentRow = require(
  "../../../scripts/designview/signatoryattachments/signatoryattachmentrow"
);
var Signatory = require("../../../js/signatories.js").Signatory;

var WrapperComponent = React.createClass({
  render: function () {
    var rowProps = _.extendOwn({}, {key: "row", ref: "row"}, this.props);

    return React.createElement(
      "table", {},
      [
        React.createElement(
          "tbody", {key: "tbody"},
          [React.createElement(SignatoryAttachmentRow, rowProps)]
        )
      ]
    );
  }
});

describe("designview/signatoryattachments/signatoryattachmentrow", function () {
  var container = null;
  var document_ = null;
  var attachment = null;
  var signatory = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var defaultProps = {
      attachment: attachment,
      signatories: document_.signatories(),
      onRemove: sinon.stub()
    };

    var actualProps = _.extendOwn({}, defaultProps, props || {});

    var component = React.render(
      React.createElement(WrapperComponent, actualProps), container
    );

    return component.refs.row;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      signatory = document_.signatories()[1];
      attachment = new DesignSignatoryAttachment({
        name: "spam",
        description: "eggs",
        signatory: signatory,
        isRequired: true
      });

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

  it("should generate signatory options when it mounts", function () {
    var component = renderComponent();
    assert.notEqual(component.signatoryOptions.length, 0);
  });

  it("should start listening to attachment change event when it mounts", function () {
    sinon.stub(attachment, "on");

    var component = renderComponent();
    assert.isTrue(
      attachment.on.calledWith("change", component.onAttachmentChange)
    );
  });

  it("should generate signatory options when it receives props", function () {
    var component = renderComponent();
    sinon.stub(component, "generateSignatoryOptions");

    component.componentWillReceiveProps();
    assert.isTrue(component.generateSignatoryOptions.called);
  });

  it("should stop listening to attachment change event when it unmounts", function () {
    sinon.stub(attachment, "off");

    var component = renderComponent();
    React.unmountComponentAtNode(container);
    container = null;

    assert.isTrue(
      attachment.off.calledWith("change", component.onAttachmentChange)
    );
  });

  describe("nameFromSignatory", function () {
    it("should return name or e-mail", function () {
      sinon.stub(signatory, "nameOrEmail").returns("spam");

      var component = renderComponent();

      var name = component.nameFromSignatory(signatory);
      assert.equal(name, "spam");
    });

    it("should return generic name for signatory loaded from CSV", function () {
      sinon.stub(signatory, "nameOrEmail").returns("spam");
      sinon.stub(signatory, "isCsv").returns(true);

      var component = renderComponent();

      var name = component.nameFromSignatory(signatory);
      assert.notEqual(name, "spam");
    });

    it("should return name in document", function () {
      sinon.stub(signatory, "nameOrEmail").returns("");
      sinon.stub(signatory, "nameInDocument").returns("eggs");

      var component = renderComponent();

      var name = component.nameFromSignatory(signatory);
      assert.equal(name, "eggs");
    });
  });

  it("should generate signatory options", function () {
    var component = renderComponent();

    var signatoryOptions = component.generateSignatoryOptions();
    assert.equal(signatoryOptions.length, 1);

    var signatoryOption = signatoryOptions[0];
    assert.equal(signatoryOption.name, "Party 2");
    assert.equal(signatoryOption.value, document_.signatories()[1]);
  });

  it("should render name input", function () {
    var component = renderComponent();

    var input = TestUtils.findRenderedDOMComponentWithClass(
      component, "editSignatoryAttachmentName"
    );

    assert.equal(input.props.value, attachment.get("name"));
  });

  it("should update the attachment's name when name input changes", function () {
    sinon.stub(attachment, "set");

    var component = renderComponent();

    var input = TestUtils.findRenderedDOMComponentWithClass(
      component, "editSignatoryAttachmentName"
    );

    input.getDOMNode().value = "eggs";
    TestUtils.Simulate.change(input);

    assert.isTrue(attachment.set.calledWith("name", "eggs"));
  });

  it("should render description input", function () {
    var component = renderComponent();

    var input = TestUtils.findRenderedDOMComponentWithClass(
      component, "editSignatoryAttachmentDescription"
    );

    assert.equal(input.props.value, attachment.get("description"));
  });

  it("should update the attachment's description when description input changes", function () {
    sinon.stub(attachment, "set");

    var component = renderComponent();

    var input = TestUtils.findRenderedDOMComponentWithClass(
      component, "editSignatoryAttachmentDescription"
    );

    input.getDOMNode().value = "eggs";
    TestUtils.Simulate.change(input);

    assert.isTrue(attachment.set.calledWith("description", "eggs"));
  });

  it("should render icon for required attachment", function () {
    var component = renderComponent();

    var icon = TestUtils.findRenderedDOMComponentWithClass(
      component, "signatory-required-attachment-icon"
    );

    assert.isFalse(
      icon.getDOMNode().classList.contains("signatory-optional-attachment-icon")
    );
  });

  it("should render icon for optional attachment", function () {
    attachment.set("isRequired", false);

    var component = renderComponent();

    var icon = TestUtils.findRenderedDOMComponentWithClass(
      component, "signatory-optional-attachment-icon"
    );

    assert.isFalse(
      icon.getDOMNode().classList.contains("signatory-required-attachment-icon")
    );
  });

  it("should render the isRequired select", function () {
    var component = renderComponent();

    var select = component.refs.selectIsRequired;
    assert.equal(select.props.options.length, 2);
    assert.isTrue(select.props.options[0].value);
    assert.isFalse(select.props.options[1].value);
    assert.equal(select.props.onSelect, component.onIsRequiredSelectChange);
  });

  it("should update the attachment's isRequired flag when isRequired select changes", function () {
    sinon.stub(attachment, "set");

    var component = renderComponent();

    component.onIsRequiredSelectChange(false);
    assert.isTrue(attachment.set.calledWith("isRequired", false));
  });

  it("should render the signatory select", function () {
    var component = renderComponent();

    var select = component.refs.selectSignatory;
    assert.equal(select.props.options.length, component.signatoryOptions.length);
    assert.equal(select.props.options[0], component.signatoryOptions[0]);
    assert.equal(select.props.onSelect, component.onSignatorySelectChange);
  });

  it("should update the attachment's signatory when signtatory select changes", function () {
    sinon.stub(attachment, "set");

    var component = renderComponent();

    var newSignatory = new Signatory({});
    component.onSignatorySelectChange(newSignatory);

    assert.isTrue(attachment.set.calledWith("signatory", newSignatory));
  });

  it("should render the remove icon", function () {
    var component = renderComponent();

    var icons = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "removeSignatoryAttachmentIcon"
    );

    assert.equal(icons.length, 1);
  });

  it("should call the onRemove callback when remove icon is clicked", function () {
    var component = renderComponent();

    var icon = TestUtils.findRenderedDOMComponentWithClass(
      component, "removeSignatoryAttachmentIcon"
    );

    TestUtils.Simulate.click(icon);
    assert.isTrue(component.props.onRemove.calledWith(attachment));
  });
});
