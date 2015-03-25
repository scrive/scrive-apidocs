define(["legacy_code", "backend", "util", "React", "designview/typesetters/typesettermixin", "common/editabletext", "common/button"], function(legacy_code, backend, util, React, TypeSetterMixin, EditableText, Button) {

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

        var typesetter = placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement
          , element: $("body")[0]
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

        assert.equal(placement.typeSetter, undefined, "placement typeSetter should be unset");
      });

      it("should test mixin errors", function () {
        var placement1 = util.addPlacement(doc, 1);
        var placement2 = util.addPlacement(doc, 2);

        var TestComponent1 = React.createClass({
          mixins: [TypeSetterMixin]
        });

        assert.throws(function () {
          var typesetter = placement1.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent1, {
            model: placement1
            , element: $("body")[0]
          }));
        }, /renderBody/);

        var TestComponent2 = React.createClass({
          mixins: [TypeSetterMixin]
          , renderBody: function () {
            return React.createElement("span");
          }
        });

        var typesetter2 = placement2.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent2, {
          model: placement2
          , element: $("body")[0]
        }));

        typesetter2.rename("field-1");

        assert.equal(placement2.field().name(), "field-2", "should not have renamed typesetter");
      });

      it("should test clearing all typesetters", function () {
        var placement1 = util.addPlacement(doc);
        var placement2 = util.addPlacement(doc);

        var TestComponent = React.createClass({
          mixins: [TypeSetterMixin]
          , renderBody: function () { }
        });

        placement1.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement1
          , element: $("body")[0]
        }));

        placement2.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement2
          , element: $("body")[0]
        }));

        assert.equal(placement1.typeSetter, undefined, "placement1's typesetter has been unset.");
      });

      it("should test clearing a typesetter when signatory is removed", function () {
        var placement = util.addPlacement(doc, 1, 1);

        var TestComponent = React.createClass({
          mixins: [TypeSetterMixin]
          , renderBody: function () { }
        });

        var typesetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement
          , element: $("body")[0]
        }));

        var sig = placement.field().signatory();
        var field = placement.field();

        assert.ok(typesetter.isMounted(), "typesetter should be mounted");

        sig.removed();

        typesetter.forceUpdate(); // backbone mixin does not capture removed event.

        assert.ok(!typesetter.isMounted(), "typesetter should not be mounted");
      });

      it("should test clearing a typesetter when signatory is removed", function () {
        var placement = util.addPlacement(doc);

        var TestComponent = React.createClass({
          mixins: [TypeSetterMixin]
          , renderBody: function () { }
        });

        var typesetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement
          , element: $("body")[0]
        }));

        var sig = placement.field().signatory();
        var field = placement.field();

        assert.ok(typesetter.isMounted(), "typesetter should be mounted");

        sig.deleteField(field);

        assert.ok(!typesetter.isMounted(), "typesetter should not be mounted");
      });

      it("should test rename", function () {
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
});
