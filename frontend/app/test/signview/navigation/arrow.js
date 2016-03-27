var backend = require("../../backend");
var util = require("../../util");
var ImageShim = require("../../image");
var React = require("react");
var Arrow = require("../../../scripts/signview/navigation/arrow");
var Task = require("../../../scripts/signview/navigation/task");

var TestUtils = React.addons.TestUtils;

describe("signview/navigation/arrow", function () {
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

  describe("Arrow", function () {
    it("should test component", function () {
      var task = new Task({
        type: "field",
        isComplete: function () {
          return false;
        },
        el: $("body"),
        onArrowClick: function () {
        },
        tipSide: "right"
      });

      var arrow = TestUtils.renderIntoDocument(React.createElement(Arrow, {
        show: true,
        task: task
      }));
    });
  });

  after(function () {
    window.Image = oldImage;
    server.restore();
  });
});
