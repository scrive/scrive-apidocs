define(["legacy_code", "backend", "util", "React", "signview/identify/norwegianidentify",
        "signview/identify/norwegianprocessing", "signview/identify/norwegianproblem",
        "common/button", "common/infotextinput"],
  function(legacy_code, backend, util, React, NorwegianIdentify, NorwegianProcessing, NorwegianProblem,
           Button, InfoTextInput) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/identify/norwegian", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("Identify", function () {
      it("should test Identify without phone number", function (done) {
        var phoneNumber = "12345678";
        var identify = TestUtils.renderIntoDocument(React.createElement(NorwegianIdentify, {
          onIdentify: function (type, phone) {
            assert.equal(type, "mobile", "type should be mobile");
            assert.equal(phone, phoneNumber, "phone number should be set");
            done();
          },
          personalNumber: "12345",
          phoneNumber: ""
        }));

        var buttons = util.findComponents(identify, Button);

        assert.equal(buttons.length, 3, "there should be 3 buttons");

        var desktopButton = buttons[0];
        var mobileButton = buttons[1];
        var identifyButton = buttons[2];

        assert.equal(identify.state.type, "desktop", "desktop should be default");

        TestUtils.Simulate.click(mobileButton.getDOMNode());

        assert.equal(identify.state.type, "mobile", "type should be mobile");

        TestUtils.Simulate.click(desktopButton.getDOMNode());

        assert.equal(identify.state.type, "desktop", "type should be desktop");

        TestUtils.Simulate.click(mobileButton.getDOMNode());

        var textInput = TestUtils.findRenderedDOMComponentWithTag(identify, "input");

        assert.ok(textInput, "there should be a text input");

        // invalid phone number
        TestUtils.Simulate.click(identifyButton.getDOMNode());

        assert.ok($(".flash").length > 0, "there should be a flashmessage");

        // add valid phone number
        TestUtils.Simulate.focus(textInput);

        TestUtils.Simulate.change(textInput, {target: {value: phoneNumber}});

        TestUtils.Simulate.blur(textInput);

        TestUtils.Simulate.click(identifyButton.getDOMNode());
      });

      it("should test Identify with phone number", function (done) {
        var phoneNumber = "12345678";
        var identify = TestUtils.renderIntoDocument(React.createElement(NorwegianIdentify, {
          onIdentify: function (type, phone) {
            assert.equal(type, "mobile", "type should be mobile");
            assert.equal(phone, phoneNumber, "phone number should be set");
            done();
          },
          personalNumber: "12345",
          phoneNumber: phoneNumber
        }));

        var buttons = util.findComponents(identify, Button);

        assert.equal(buttons.length, 3, "there should be 3 buttons");

        var desktopButton = buttons[0];
        var mobileButton = buttons[1];
        var identifyButton = buttons[2];

        assert.equal(identify.state.type, "desktop", "desktop should be default");

        TestUtils.Simulate.click(mobileButton.getDOMNode());

        assert.equal(identify.state.type, "mobile", "type should be mobile");

        TestUtils.Simulate.click(desktopButton.getDOMNode());

        assert.equal(identify.state.type, "desktop", "type should be desktop");

        TestUtils.Simulate.click(mobileButton.getDOMNode());

        TestUtils.Simulate.click(identifyButton.getDOMNode());
      });
    });

    describe("Processing", function () {
      it("should test Processing", function () {
        TestUtils.renderIntoDocument(React.createElement(NorwegianProcessing));
      });
    });

    describe("Problem", function () {
      it("should test Problem", function (done) {
        var problem = TestUtils.renderIntoDocument(React.createElement(NorwegianProblem, {
          onBack: done
        }));

        var ok = util.findComponents(problem, Button)[0];

        assert.ok(ok, "there should be a button");

        TestUtils.Simulate.click(ok.getDOMNode());
      });
    });

    after(function () {
      server.restore();
    });
  });
});
