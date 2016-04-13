var React = require("react");
var Select = require("../../scripts/common/select");
// Some trivial test of the common React components.


  var TestUtils = React.addons.TestUtils;

  describe("common/select", function () {
    it("should test selecting an option (width global onSelect)", function (done) {
      var options = [
        {
          name: "zero",
          value: "A"
        },
        {
          name: "one",
          value: "B",
          selected: true
        },
        {
          name: "two",
          value: "C"
        }
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        options: options,
        onSelect: function (v) {
          assert.equal(v, "C", "should be the index we choose");
          done();
        }
      }));

      var node = select.getDOMNode();
      assert.ok($("select",node).length === 1 , "there should be select component")
      assert.ok($("select option",node).length === 3 , "it should have 3 options inside")
      assert.ok($("select",node).val() === "1" , "we should have snd option selected (index 1)")
      select.select(2);
    });

    it("should test selecting an option (with local onSelect)", function (done) {
      var options = [
        {
          name: "zero",
          value: "A",
          selected: true
        },
        {
          name: "one",
          value: "B"
        },
        {
          name: "two",
          value: "C",
          onSelect: function (v) {
            assert.equal(v, "C", "should be the index we choose");
            done();
          }
        }
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        options: options,
        onSelect: function (v) {
          assert.ok(false , "onSelect for options should have higher priority then onSelect for select")
        }
      }));

      var node = select.getDOMNode();
      assert.ok($("select",node).val() === "0" , "we should have snd option selected (index 0)")
      select.select(2);
    });

    it("should test disabled options is still rendered", function () {
      var options = [
        {
          name: "zero",
          value: "A",
          disabled: true
        },
        {
          name: "one",
          value: "B",
          disabled: true
        },
        {
          name: "two",
          value: "C"
        }
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        options: options,
        onSelect: function (v) {
        }
      }));

      var node = select.getDOMNode();
      assert.ok(!$(node).hasClass("inactive") , "it should not have inactive class");
      assert.ok($("select option",node).length === 3 , "it should have 3 options inside");
      assert.ok($("select option:disabled",node).length === 2, "two options should be disabled");
    });

    it("should test inactive select when only one option", function () {
      var options = [
        {
          name: "zero",
          value: "A",
          disabled: true
        }
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        options: options,
        onSelect: function (v) {
        }
      }));

      var node = select.getDOMNode();
      assert.ok($(node).hasClass("inactive") , "it should have inactive class");
    });

    it("should use className parameter", function () {
      var options = [
        {
          name: "zero",
          value: "A",
          disabled: true
        }
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        options: options,
        className: "test-class",
        onSelect: function (v) {
        }
      }));

      var node = select.getDOMNode();
      assert.ok($(node).hasClass("test-class") , "it should have test-class class");
    });

    it("should test have remove option when onRemove is set", function (done) {
      var options = [
        {
          name: "zero",
          value: "A",
          disabled: true
        }
      ];

      var select = TestUtils.renderIntoDocument(React.createElement(Select, {
        options: options,
        onRemove: function() {
          done();
        },
        onSelect: function (v) {
        }
      }));

      var node = select.getDOMNode();
      assert.ok($(".closer",node).length === 1 , "it should have closer");
      TestUtils.Simulate.click($(".closer",node)[0]);
    });

  });
