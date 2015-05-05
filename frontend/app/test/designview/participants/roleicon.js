define(["legacy_code", "backend", "util", "React", "designview/participants/roleicon"], function(legacy_code, backend, util, React, RoleIcon) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/roleicon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should role icon changes signatory role method when clicking", function () {

      var orderIcon = TestUtils.renderIntoDocument(React.createElement(RoleIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var signs1 = signatory.signs()
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      var signs2 = signatory.signs()
      assert.notEqual(signs1, signs2);
      orderIcon.forceUpdate();
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      var signs3 = signatory.signs()
      assert.equal(signs3, signs1);
    });


    after(function () {
      server.restore();
    });
  });
});
