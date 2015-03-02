define(["legacy_code", "backend", "util", "React", "designview/typesetters/typesettermixin"], function(legacy_code, backend, util, React, TypeSetterMixin) {

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
          , renderTitle: function () {
            return React.createElement("span", { className: "title-1" });
          }
          , renderBody: function () {
            return React.createElement("span", { className: "body-1" });
          }
        });

        var typesetter = placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent, {
          model: placement
          , element: $("body")[0]
        }));

        var title = TestUtils.findRenderedDOMComponentWithClass(typesetter, "title-1");

        assert.ok(title, "there should be a title.");

        var body = TestUtils.findRenderedDOMComponentWithClass(typesetter, "body-1");

        assert.ok(body, "there should be a body.");

        // done typesetter.
        typesetter.done();

        assert.equal(placement.typeSetter, undefined, "placement typeSetter should be unset");
      });

      it("should test mixin errors", function () {
        var placement1 = util.addPlacement(doc);
        var placement2 = util.addPlacement(doc);

        var TestComponent1 = React.createClass({
          mixins: [TypeSetterMixin]
        });

        assert.throws(function () {
          var typesetter = placement1.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent1, {
            model: placement1
            , element: $("body")[0]
          }));
        }, /renderTitle/);

        var TestComponent2 = React.createClass({
          mixins: [TypeSetterMixin]
          , renderTitle: function () { }
        });

        assert.throws(function () {
          var typesetter = placement2.typeSetter = TestUtils.renderIntoDocument(React.createElement(TestComponent2, {
            model: placement2
            , element: $("body")[0]
          }));
        }, /renderBody/);
      });

      it("should test clearing all typesetters", function () {
        var placement1 = util.addPlacement(doc);
        var placement2 = util.addPlacement(doc);

        var TestComponent = React.createClass({
          mixins: [TypeSetterMixin]
          , renderTitle: function () { }
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
    });

    after(function () {
      server.restore();
    });
  });
});
