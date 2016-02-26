var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var AnchorTable = require("../../../scripts/designview/typesetters/anchortableview");

  var TestUtils = React.addons.TestUtils;

  describe('designview/typesetters/anchortableview', function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe('AnchorTable', function () {
      it('should test component with empty input', function () {
        var anchorTable = TestUtils.renderIntoDocument(React.createElement(AnchorTable, {
          anchors: []
        }));

        assert.equal(anchorTable.getAnchors().length, 1, "there should be one anchor added by default");

        anchorTable.removeAnchor(0);

        assert.equal(anchorTable.getAnchors().length, 0, "there should be no anchors");
      });
    });

    after(function () {
      server.restore();
    });
  });
