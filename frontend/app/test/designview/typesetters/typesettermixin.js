var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var TypeSetterMixin = require("../../../scripts/designview/typesetters/typesettermixin");
var EditableText = require("../../../scripts/designview/typesetters/editabletext");
var Button = require("../../../scripts/common/button");

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/typesettermixin", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("TypeSetterMixin", function () {
      it("should test mixin", function () {
        var placement = util.addPlacement(doc);

        var TestComponent = React.createClass({
          mixins: [TypeSetterMixin]
          , renderBody: function () {
            return React.createElement("span", { className: "body-1" });
          }
        });

        var typesetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
            model: placement
          , element: $("body")[0]
          , hideFunc: function() {}
        }));

        var title = TestUtils.findRenderedDOMComponentWithClass(typesetter, "title");

        assert.ok(title, "there should be a title.");

        var body = TestUtils.findRenderedDOMComponentWithClass(typesetter, "body-1");

        assert.ok(body, "there should be a body.");

        // place typesetter
        typesetter.place();

        // rename typesetter
        typesetter.rename("new-field-1");

        assert.equal(placement.field().name(), "new-field-1", "should have renamed typesetter");

        // done typesetter.
        typesetter.done();

      });

      xit("should test mixin errors", function () {
        var placement1 = util.addPlacement(doc, 1);
        var placement2 = util.addPlacement(doc, 2);

        var TestComponent1 = React.createClass({
          mixins: [TypeSetterMixin]
        });

        assert.throws(function () {
          var typesetter = TestUtils.renderIntoDocument(React.createElement(TestComponent1, {
              model: placement1
            , element: $("body")[0]
            , hideFunc: function() {}
          }));
        }, /renderBody/);

        var TestComponent2 = React.createClass({
            mixins: [TypeSetterMixin]
          , renderBody: function () {
            return React.createElement("span");
          }
        });

        var typesetter2 = TestUtils.renderIntoDocument(React.createElement(TestComponent2, {
            model: placement2
          , element: $("body")[0]
          , hideFunc: function() {}

        }));

        typesetter2.rename("field-1");

        assert.equal(placement2.field().name(), "field-2", "should not have renamed typesetter");
      });

      xit("should test rename", function () {
        var placement = util.addPlacement(doc, undefined, undefined, {
          type: "standard"
        });

        var TestComponent = React.createClass({
          mixins: [TypeSetterMixin]
          , renderBody: function () { }
        });

        var typesetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement
          , element: $("body")[0]
          , hideFunc: function() {}
        }));

        var sig = placement.field().signatory();
        var field = placement.field();

        assert.equal(typesetter.rename(""), true, "rename '' should return true");
      });
    });

    after(function () {
      server.restore();
    });
  });
