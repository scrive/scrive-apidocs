var React = require("react");
var EditableText = require("../../../scripts/designview/typesetters/editabletext");
var Button = require("../../../scripts/common/button");

var TestUtils = React.addons.TestUtils;

describe("designview/typesetters/editabletext", function () {
  it("should test the onChange handler", function () {
    var originalText = "text";
    var newText = "new text";
    var onChange = sinon.spy();

    var editable = TestUtils.renderIntoDocument(React.createElement(EditableText, {
      edit: true,
      text: originalText,
      onChange: onChange
    }));

    var input = TestUtils.findRenderedDOMComponentWithTag(editable, "input");
    TestUtils.Simulate.change(input, {target: {value: newText}});
    assert.isTrue(onChange.calledWith(newText));
  });

  it("should test the disabled prop", function () {
    var originalText = "text";
    var newText = "new text";

    var editable = TestUtils.renderIntoDocument(React.createElement(EditableText, {
      text: originalText,
      disabled: false,
      onChange: sinon.spy()
    }));

    TestUtils.Simulate.click(editable.getDOMNode());

    var inputs1 = TestUtils.scryRenderedDOMComponentsWithTag(editable, "input");
    assert.equal(inputs1.length, 1, "there should be one input");

    editable.setProps({ disabled: true });

    var inputs2 = TestUtils.scryRenderedDOMComponentsWithTag(editable, "input");
    assert.equal(inputs2.length, 0, "there should no buttons");
  });
});
