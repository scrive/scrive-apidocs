define(["legacy_code", "React", "common/editabletext", "common/button"], function(undefined, React, EditableText, Button) {

  var TestUtils = React.addons.TestUtils;

  describe("common/editabletext", function () {
    it("should test the onSave handler", function () {
      var originalText = "text";
      var newText = "new text";

      var editable = TestUtils.renderIntoDocument(React.createElement(EditableText, {
        text: originalText,
        onSave: function (text) {
          assert.equal(text, newText, "text should have been changed");
          return true;
        }
      }));

      TestUtils.Simulate.click(editable.getDOMNode());

      var buttons = TestUtils.findAllInRenderedTree(editable, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Button);
      });

      assert.equal(buttons.length, 1, "there should be only one button");

      var save = buttons[0];

      var input = TestUtils.findRenderedDOMComponentWithTag(editable, "input");

      TestUtils.Simulate.change(input, { target: { value: newText } });

      TestUtils.Simulate.click(save.getDOMNode());
    });

    it("should test the disabled prop", function () {
      var originalText = "text";
      var newText = "new text";

      var editable = TestUtils.renderIntoDocument(React.createElement(EditableText, {
        text: originalText,
        disabled: false,
        onSave: function (text) {
          assert.equal(text, newText, "text should have been changed");
          return true;
        }
      }));

      TestUtils.Simulate.click(editable.getDOMNode());

      var buttons1 = TestUtils.findAllInRenderedTree(editable, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Button);
      });

      assert.equal(buttons1.length, 1, "there should be one button");

      editable.setProps({ disabled: true });

      var buttons2 = TestUtils.findAllInRenderedTree(editable, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Button);
      });

      assert.equal(buttons2.length, 0, "there should no buttons");
    });
  });
});
