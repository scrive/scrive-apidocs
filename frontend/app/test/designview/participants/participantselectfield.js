var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var ParticipantSelectField = require("../../../scripts/designview/participants/participantselectfield");
var Field = require("../../../js/fields").Field;

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
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      });
      signatory.addField(field);
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantSelectField, {
          model: field,
          element: $("body")[0]
        }));

      fieldView.refs["select"].select(1);
      //This depends a lot on document structure
      assert.equal(field.type(),"text");
    });

   it("should test component  with using enter and same name conflict", function () {
     var field = new Field({
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
      fieldView.refs["select"].select(1);
      assert.equal(field.type(),"text");
      assert.equal(field.name(),"");
    });

    it("should test component with removing field", function () {
      util.cleanTimeoutsAndBody();
      var field = new Field({
        name: "Sample name",
        type: "text",
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
      TestUtils.Simulate.click(fieldView.refs["select"].refs["close"].getDOMNode());
      assert.notEqual(signatory.fields().length,signatoryFieldsCount);
    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
