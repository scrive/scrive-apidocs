var backend = require("../../backend");
var util = require("../../util");
var ImageShim = require("../../image");
var React = require("react");
var Page = require("../../../scripts/signview/pageviewer/page");

var TestUtils = React.addons.TestUtils;

describe("signview/pageviewer/page", function () {
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

  describe("Page", function () {
    it("should test component", function () {
      var page = TestUtils.renderIntoDocument(React.createElement(Page, {
        number: 0,
        imgSrc: "",
        width: 0
      }));
    });
  });

  after(function () {
    window.Image = oldImage;
    server.restore();
  });
});
