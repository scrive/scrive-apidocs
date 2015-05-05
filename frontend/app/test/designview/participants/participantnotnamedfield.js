define(["legacy_code", "backend", "util", "React", "designview/participants/participantnotnamedfield"], function(legacy_code, backend, util, React, ParticipantNotNamedField) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participantnotnamedfield", function () {
    var server, designView, signatory;

    before(function (done) {
      util.cleanTimeoutsAndBody();
      server = backend.createServer();
      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should test component with setting name", function () {
      var field = new Field({
        name: "",
        type: "custom",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      });
      signatory.addField(field);
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantNotNamedField, {
          model: field,
          element: $("body")[0]
        }));
      TestUtils.Simulate.change(fieldView.refs["input"].refs["input"].getDOMNode(),{target: {value: "Field name"}});
      assert.equal(field.name(),"");
      TestUtils.Simulate.click(fieldView.refs["button"].getDOMNode());
      assert.equal(field.name(),"Field name");
    });

    it("should test component  with using enter and same name conflict", function () {
      util.cleanTimeoutsAndBody();
      var field = new Field({
        name: "",
        type: "custom",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      });
      signatory.addField(field);
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantNotNamedField, {
          model: field,
          element: $("body")[0]
        }));
      TestUtils.Simulate.change(fieldView.refs["input"].refs["input"].getDOMNode(),{target: {value: "Field name"}});
      // Should fail since we are using same name
      TestUtils.Simulate.click(fieldView.refs["button"].getDOMNode());
      TestUtils.Simulate.keyDown(fieldView.refs["input"].refs["input"].getDOMNode(),{keyCode: 13});
      assert.equal(field.name(),"");

      TestUtils.Simulate.change(fieldView.refs["input"].refs["input"].getDOMNode(),{target: {value: "Field name 2"}});
      TestUtils.Simulate.keyDown(fieldView.refs["input"].refs["input"].getDOMNode(),{keyCode: 13});
      assert.equal(field.name(),"Field name 2");
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
      var fieldView = TestUtils.renderIntoDocument(React.createElement(ParticipantNotNamedField, {
          model: field,
          element: $("body")[0]
        }));
      var signatoryFieldsCount = signatory.fields().length;
      TestUtils.Simulate.click(fieldView.refs["input"].refs["close"].getDOMNode());
      assert.notEqual(signatory.fields().length,signatoryFieldsCount);
    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
});
