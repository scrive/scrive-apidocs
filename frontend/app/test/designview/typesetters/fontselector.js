var imports = ["legacy_code", "backend", "util", "React", "common/select",
               "common/button", "designview/typesetters/fontselector"];

define(imports, function(legacy_code, backend, util, React, Select, Button, FontSelector) {
  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/fontselector", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    it("should test font selector options", function () {
      var placement = util.addPlacement(doc);
      var field = placement.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(placement.get("page"));

      var fontSelector = TestUtils.renderIntoDocument(React.createElement(FontSelector, {
        model: placement
      }));

      var select = TestUtils.findAllInRenderedTree(fontSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      assert.ok(select, "there should be a select");

      // there are four font sizes and we should click them all.
      util.clickSelectOption(select, 0);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeSmall / page.width(), "font should be small");
      fontSelector.forceUpdate();

      util.clickSelectOption(select, 1);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeNormal / page.width(), "font should be normal");
      fontSelector.forceUpdate();

      util.clickSelectOption(select, 2);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeLarge / page.width(), "font should be large");
      fontSelector.forceUpdate();

      util.clickSelectOption(select, 3);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeHuge / page.width(), "font should be huge");
      fontSelector.forceUpdate();

      // custom font
      placement.setFSRel((FieldPlacementGlobal.fontSizeHuge + 10) / page.width());
      fontSelector.forceUpdate();
    });

    it("should test font selector without page", function () {
      var placement = util.addPlacement(doc);

      placement.set({ page: null });

      var fontSelector = TestUtils.renderIntoDocument(React.createElement(FontSelector, {
        model: placement
      }));

      var select = TestUtils.findAllInRenderedTree(fontSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      assert.notOk(select, "there should not be a select");
    });

    after(function () {
      server.restore();
    });
  });
});
