var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var ExtraDetailsView = require("../../../scripts/signview/extradetails/extradetailsview");

  var TestUtils = React.addons.TestUtils;

  describe("signview/extradetails/extradetailsview", function () {
    var server, doc;

    var SignView = Backbone.Model.extend({
      askForName: function () {
        return true;
      },

      askForEmail: function () {
        return true;
      },

      askForSSN: function () {
        return false;
      },

      askForSSNIfNotEID: function () {
        return false;
      },

      askForPhone: function () {
        return false;
      },

      askForPhoneIfNotPin: function () {
        return false;
      }
    });

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("ExtraDetailsView", function () {
      it("should test component", function () {
        var extraView = TestUtils.renderIntoDocument(React.createElement(ExtraDetailsView, {
          model: doc.currentSignatory(),
          signview: new SignView()
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });
