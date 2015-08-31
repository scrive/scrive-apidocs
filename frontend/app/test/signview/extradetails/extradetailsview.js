define(["legacy_code", "backend", "util", "React", "signview/extradetails/extradetailsview"],
  function(legacy_code, backend, util, React, ExtradetailsView, Button) {

  var TestUtils = React.addons.TestUtils;

  var SignView = Backbone.Model.extend({
    askForName: function () { return true; },
    askForEmail: function () { return true; },
    askForPhone: function () { return true; },
    askForSSN: function () { return true; }
  });

  describe("signview/extradetails/extradetailsview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("ExtradetailsView", function () {
      it("should test ExtradetailsView", function () {
        var sig = doc.signatories()[0];
        var phoneField = sig.addNewField("standard");
        var ssnField = sig.addNewField("standard");
        phoneField.setName("mobile");
        ssnField.setName("sigpersnr");

        var signview = new SignView();

        var extradetails = TestUtils.renderIntoDocument(React.createElement(ExtradetailsView, {
          model: sig,
          signview: signview,
          askForPhone: signview.askForPhone(),
          askForSSN: signview.askForSSN(),
          askForEmail: signview.askForEmail(),
          askForName: signview.askForName()
        }));

        var inputs = TestUtils.scryRenderedDOMComponentsWithTag(extradetails, "input");

        assert.equal(inputs.length, 4, "there should be two inputs");

        var nameInput = inputs[0];
        var emailInput = inputs[1];
        var ssnInput = inputs[2];
        var phoneInput = inputs[3];

        var fstnameField = sig.fstnameField();
        var sndnameField = sig.sndnameField();
        TestUtils.Simulate.change(nameInput, {target:{value: "fst snd"}});
        assert.equal(fstnameField.value(), "fst", "first name should be fst");
        assert.equal(sndnameField.value(), "snd", "last name should be snd");
        TestUtils.Simulate.change(nameInput, {target:{value: "test"}});
        assert.equal(fstnameField.value(), "test", "first name should be test");

        var emailField = sig.emailField();
        TestUtils.Simulate.change(emailInput, {target:{value: "test@example.com"}});
        assert.equal(emailField.value(), "test@example.com", "email should be test@example.com");

        TestUtils.Simulate.change(ssnInput, {target:{value: "0101011234"}});
        assert.equal(ssnField.value(), "0101011234", "ssn should be 0101011234");

        TestUtils.Simulate.change(phoneInput, {target:{value: "1234567"}});
        assert.equal(phoneField.value(), "1234567", "ssn should be 1234567");
      });
    });


    after(function () {
      server.restore();
    });
  });
});
