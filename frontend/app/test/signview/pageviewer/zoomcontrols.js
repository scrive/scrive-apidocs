var backend = require("../../backend");
var util = require("../../util");
var ImageShim = require("../../image");
var React = require("react");
var ZoomControls = require("../../../scripts/signview/pageviewer/zoomcontrols");

var TestUtils = React.addons.TestUtils;

describe("signview/pageviewer/zoomcontrols", function () {
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

  describe("ZoomControls", function () {
    it("should test component", function () {
      var zoomControls = TestUtils.renderIntoDocument(React.createElement(ZoomControls, {
        show: true,
        canZoomIn: true,
        canZoomOut: true,
        onZoomIn: function () { },
        onZoomOut: function () { }
      }));
    });
  });

  after(function () {
    window.Image = oldImage;
    server.restore();
  });
});
