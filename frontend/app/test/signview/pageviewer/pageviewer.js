var backend = require("../../backend");
var util = require("../../util");
var ImageShim = require("../../image");
var React = require("react");
var PageViewer = require("../../../scripts/signview/pageviewer/pageviewer");

var TestUtils = React.addons.TestUtils;

describe("signview/pageviewer/pageviewer", function () {
  var server, doc, oldImage;

  before(function (done) {
    server = backend.createServer();
    oldImage = window.Image;
    window.Image = ImageShim;
    util.createDocument(function (d) {
      doc = d;
      done();
    });
  });

  describe("PageViewer", function () {
    it("should test component", function () {
      var pageViewer = TestUtils.renderIntoDocument(React.createElement(PageViewer, {
        ready: true,
        showOverlay: false,
        showArrow: false
      }));
    });
  });

  after(function () {
    window.Image = oldImage;
    server.restore();
  });
});
