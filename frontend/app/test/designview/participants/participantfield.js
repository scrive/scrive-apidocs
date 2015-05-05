define(["legacy_code", "backend", "util", "React", "designview/participants/participantfield"], function(legacy_code, backend, util, React, ParticipantField) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participantfield", function () {
    var server, designView, signatory, author;

    before(function (done) {
      util.cleanTimeoutsAndBody();
      server = backend.createServer();
      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        author =  doc.signatories()[0]
        done();
      });
    });

    it("should test component", function () {
      var field = TestUtils.renderIntoDocument(React.createElement(ParticipantField, {
        model: signatory.emailField(),
        element: $("body")[0]
      }));
      assert.equal($(".closer",field.getDOMNode()).length,0);
      assert.equal(field.refs["input"].value(),signatory.emailField().value());
      TestUtils.Simulate.change(field.refs["input"].refs["input"].getDOMNode(),{target: {value: "axel@scrive.com"}});
      assert.equal("axel@scrive.com",signatory.emailField().value());
      signatory.setDelivery("mobile");
      field.forceUpdate();
      assert.equal($(".closer",field.getDOMNode()).length,1);
      TestUtils.Simulate.click(field.refs["input"].refs["close"].getDOMNode());
      assert.ok(signatory.emailField() == undefined);
    });


    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
});
