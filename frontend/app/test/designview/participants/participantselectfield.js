define(["legacy_code", "backend", "util", "React", "designview/participants/participantselectfield"], function(legacy_code, backend, util, React, ParticipantSelectField) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participantselectfield", function () {
    var server, designView, signatory;

    before(function (done) {
      util.cleanTimeoutsAndBody();
      server = backend.createServer();
      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should test component with selecting standard field", function () {
      var field = new Field({
        name: "",
        type: "",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      });
      signatory.addField(field);
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantSelectField, {
          model: field,
          element: $("body")[0]
        }));
      util.clickSelectOption(fieldView.refs["select"],0);
      //This depends a lot on document structure
      assert.equal(field.type(),"standard");
      assert.equal(field.name(),"sigpersnr");
    });

   it("should test component  with using enter and same name conflict", function () {
     var field = new Field({
        name: "",
        type: "",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      });
      signatory.addField(field);
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantSelectField, {
          model: field,
          element: $("body")[0]
        }));
      //This depends a lot on document structure
      util.clickSelectOption(fieldView.refs["select"],0);
      assert.equal(field.type(),"custom");
      assert.equal(field.name(),"");
    });

    it("should test component with removing field", function () {
      util.cleanTimeoutsAndBody();
      var field = new Field({
        name: "",
        type: "custom",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      });
      signatory.addField(field);
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantSelectField, {
          model: field,
          element: $("body")[0]
        }));
      var signatoryFieldsCount = signatory.fields().length;
      TestUtils.Simulate.click(fieldView.refs["select"].refs["view"].refs["close"].getDOMNode());
      assert.notEqual(signatory.fields().length,signatoryFieldsCount);
    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
});
