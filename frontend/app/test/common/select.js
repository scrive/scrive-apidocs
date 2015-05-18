// Some trivial test of the common React components.

define(["legacy_code", "React", "common/select"], function(undefined, React, Select) {

  var TestUtils = React.addons.TestUtils;

  describe("common/select", function () {
    it("should test selecting an option", function (done) {
      var index = 0;

      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        adjustHeightOnExpand: true,
        onSelect: function (v) {
          assert.equal(v, index, "should be the index we choose");
          done();
        }
      }));

      TestUtils.Simulate.click(select.getDOMNode()); // open

      assert.ok($(".select-exp").hasClass("select"), "there should be an expanded component in body")

      TestUtils.Simulate.click(select.getDOMNode()); // close

      assert.ok(!$(".select-exp").hasClass("select"), "there should not be an expanded component in body")

      TestUtils.Simulate.click(select.getDOMNode()); // open

      var expand = select.state.expandComponent;

      var lis = TestUtils.scryRenderedDOMComponentsWithTag(expand, "li");

      assert.equal(lis.length, options.length, "there should be as many items as options");

      TestUtils.Simulate.click(lis[index]);
    });

    it("should close when hovering out", function (done) {
      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function () { }
      }));

      TestUtils.Simulate.click(select.getDOMNode());

      var expand = select.state.expandComponent;

      TestUtils.SimulateNative.mouseOver(expand.getDOMNode());

      TestUtils.SimulateNative.mouseOut(expand.getDOMNode());

      setTimeout(function () {
        assert.ok(!select.state.expanded, "should not be expanded");
        done();
      }, 100);
    });

    it("should test disabled options", function (done) {
      var index = 1;

      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1, disabled: true},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function (v) {
          assert.equal(v, index + 1, "should be the index we choose");
          done();
        }
      }));

      TestUtils.Simulate.click(select.getDOMNode());

      var expand = select.state.expandComponent;

      var lis = TestUtils.scryRenderedDOMComponentsWithTag(expand, "li");

      assert.equal(lis.length, options.length - 1, "there should be on less items as options");

      TestUtils.Simulate.click(lis[index]);
    });

    it("should test precedence of onSelect", function (done) {
      var index = 1;

      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1, onSelect: function () { done() }},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function (v) {
          assert.ok(false, "should never fire");
        }
      }));

      TestUtils.Simulate.click(select.getDOMNode());

      var expand = select.state.expandComponent;

      var lis = TestUtils.scryRenderedDOMComponentsWithTag(expand, "li");

      TestUtils.Simulate.click(lis[index]);
    });
  });

  describe("common/select coverage", function () {
    it("should unmount without errors", function () {
      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function () { }
      }));

      select.open();

      React.unmountComponentAtNode($(select.getDOMNode()).parent()[0]);
    });

    it("should block open", function () {
      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onOpen: function () { return false; },
        onSelect: function () { }
      }));

      select.open();

      assert.ok(!select.state.expanded, "should not be open");
    });

    it("should not open inactive", function () {
      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        inactive: true,
        onSelect: function () { }
      }));

      select.open();

      assert.ok(!select.state.expanded, "should not be open");
    });

    it("should test without select", function () {
      var index = 0;

      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options
      }));

      TestUtils.Simulate.click(select.getDOMNode());

      var expand = select.state.expandComponent;

      var lis = TestUtils.scryRenderedDOMComponentsWithTag(expand, "li");

      TestUtils.Simulate.click(lis[index]);
    });

    it("should test blocking close", function () {
      var index = 0;

      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function () { return false; }
      }));

      TestUtils.Simulate.click(select.getDOMNode());

      var expand = select.state.expandComponent;

      var lis = TestUtils.scryRenderedDOMComponentsWithTag(expand, "li");

      TestUtils.Simulate.click(lis[index]);

      assert.ok(select.state.expanded, "should still be open");
    });

    it("should not close when hovering out of pad device", function (done) {
      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var oldPadDevice = BrowserInfo.isPadDevice;
      BrowserInfo.isPadDevice = function () {
        return true;
      };

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function () { }
      }));

      TestUtils.Simulate.click(select.getDOMNode());

      var expand = select.state.expandComponent;

      TestUtils.SimulateNative.mouseOver(expand.getDOMNode());

      TestUtils.SimulateNative.mouseOut(expand.getDOMNode());

      setTimeout(function () {
        assert.ok(select.state.expanded, "should not be expanded");
        BrowserInfo.isPadDevice = oldPadDevice;
        done();
      }, 100);
    });

    it("should not select out of index option", function () {
      var options = [
        {name: "zero", value: 0},
        {name: "one", value: 1},
        {name: "two", value: 2}
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        name: "one",
        options: options,
        onSelect: function () { }
      }));

      select.select(3);
    });
  });
});
