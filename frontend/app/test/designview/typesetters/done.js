define(["legacy_code", "backend", "util", "React", "designview/typesetters/done"], function(legacy_code, backend, util, React, Done) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/done", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("Done", function () {
      it("should test component", function (done) {
        var placement = util.addPlacement(doc);
        var field = placement.field();

        var doneComponent = TestUtils.renderIntoDocument(React.createElement(Done, {
          field: field
          , onDone: done // finish when the test on done.
        }));

        var button = TestUtils.findRenderedDOMComponentWithTag(doneComponent, "a");

        // click the done button
        TestUtils.Simulate.click(button);
      });

      it("should test component when not done", function () {
        var placement1 = util.addPlacement(doc, 1);
        var placement2 = util.addPlacement(doc, 1);
        var field = placement1.field();

        var doneComponent = TestUtils.renderIntoDocument(React.createElement(Done, {
          field: field
          , onDone: function () {
            throw Error("done should not fire if there are two fields with the same name");
          }
        }));

        var button = TestUtils.findRenderedDOMComponentWithTag(doneComponent, "a");

        // click the done button
        TestUtils.Simulate.click(button);
      });
    });

    after(function () {
      server.restore();
    });
  });
});
