// Some trivial test of the common React components.

define(["legacy_code", "React", "designview/processsettings/customtexteditor"], function(undefined, React, CustomTextEditor) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/processsettings/customtexteditor", function () {
    it("should test everything in CustomTextEditor is rendered as expected and events are called as expected", function () {
      var previewClicked = false;
      var valueChanged = false;
      var newValue = "";
      var textEditor = TestUtils.renderIntoDocument(CustomTextEditor({
        onChange: function(v) { valueChanged = true;newValue = v;},
        onPreview: function() { previewClicked = true;},
        editable : true,
        customtext: "Value",
        label : "Label",
        placeholder: "Placeholder",
        disabledPlaceholder : "Placeholder if disabled",
        previewLabel : "Preview"
      }));
      assert.ok(previewClicked == false);
      assert.ok(valueChanged == false);
      assert.ok(newValue == "");

      var label = TestUtils.findRenderedDOMComponentWithClass(textEditor, "label");
      assert.equal($(label.getDOMNode()).text(),"Label");

      var preview = TestUtils.findRenderedDOMComponentWithClass(textEditor, "preview");
      assert.equal($(preview.getDOMNode()).text(),"Preview");

      var textarea = TestUtils.findRenderedDOMComponentWithTag(textEditor, "textarea");
      assert.equal($(textarea.getDOMNode()).val(),"Value");
      assert.equal($(textarea.getDOMNode()).attr("placeholder"),"Placeholder");

      TestUtils.Simulate.click(preview);
      assert.ok(previewClicked == true);
      TestUtils.Simulate.change(textarea.getDOMNode(),{target: {value: "New text"}});
      assert.ok(valueChanged == true);
      assert.ok(newValue == "New text");
    });

    it("should test CustomTextEditor that is disabled doesn't cause change and uses disabled label ", function () {
      var valueChanged = false;
      var textEditor = TestUtils.renderIntoDocument(CustomTextEditor({
        onChange: function(v) { valueChanged = true;},
        onPreview: function() {},
        editable : false,
        customtext: "",
        label : "Label",
        placeholder: "Placeholder",
        disabledPlaceholder : "Placeholder if disabled",
        previewLabel : "Preview"
      }));

      var textareaWrapper = TestUtils.findRenderedDOMComponentWithClass(textEditor, "custommessageeditboxwrapper");
      assert.ok($(textareaWrapper.getDOMNode()).hasClass("disabled"));


      var textarea = TestUtils.findRenderedDOMComponentWithTag(textEditor, "textarea");
      assert.equal($(textarea.getDOMNode()).val(),"Placeholder if disabled");
      assert.equal($(textarea.getDOMNode()).attr("disabled"),"disabled");

      TestUtils.Simulate.change(textarea.getDOMNode(),{target: {value: "New text"}});
      assert.ok(valueChanged == false);
    });
  });
});
