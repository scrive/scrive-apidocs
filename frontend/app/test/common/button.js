// Some trivial test of the common React components.

define(["legacy_code", "React", "common/button"], function(undefined, React, Button) {

  var TestUtils = React.addons.TestUtils;

  describe("common/button", function () {
    it("should test the onClick prop", function (done) {
      var button = TestUtils.renderIntoDocument(React.createElement(Button, {
        text: "Test"
        , onClick: done
        , width: 100
        , size: "big"
      }));

      var anchor = TestUtils.findRenderedDOMComponentWithTag(button, "a");

      assert.ok(!button.state.clicked);
      TestUtils.Simulate.click(anchor);
    });

    it("should test the oneClick prop", function () {
      var clickCount = 0;
      var button = TestUtils.renderIntoDocument(React.createElement(Button, {
        text: ["Test", "Button"]
        , oneClick: true
        , onClick: function () { clickCount++; assert.equal(clickCount, 1); }
        , size: "tiny"
        , multiline: true
        , width: 200
      }));

      var anchor = TestUtils.findRenderedDOMComponentWithTag(button, "a");

      TestUtils.Simulate.click(anchor);
      TestUtils.Simulate.click(anchor);
    });
  });
});
