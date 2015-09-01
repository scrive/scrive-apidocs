define(["legacy_code", "backend", "util", "React", "signview/signsection/signsectionview"],
  function(legacy_code, backend, util, React, SignSectionView) {

  var TestUtils = React.addons.TestUtils;

  var SignView = Backbone.Model.extend({
    setDocument: function (doc) {
      this.set({document: doc});
    },

    document: function () {
      return this.get("document");
    },

    hasRejectOption: function () {
      return true;
    },

    tasks: function () {
      return {
        notCompletedTasks: function () {
          return [{isSignTask: function () { return true; }}];
        }
      };
    },

    arrow: function () {
      return {
        blink: function () { },
        disable: function () { },
        enable: function () { }
      };
    }
  });

  describe("signview/signsection/signsectionview", function () {
    var server, doc, oldImage;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("SignSectionView", function () {
      it("should test component in full screen", function () {
        var signview = new SignView();
        signview.setDocument(doc);

        var sectionView = TestUtils.renderIntoDocument(React.createElement(SignSectionView, {
          model: signview,
          noScreenshot: true
        }));

        TestUtils.Simulate.click(sectionView.signButtonNode());
        TestUtils.Simulate.click(sectionView.refs.rejectButton.getDOMNode());
      });

      it("should test component in small screen", function () {
        var oldBrowserInfo = window.BrowserInfo;
        window.BrowserInfo = {
          isSmallScreen: function () { return true; },
          isPadDevice: function () { true; }
        };

        var signview = new SignView();
        signview.setDocument(doc);

        var sectionView = TestUtils.renderIntoDocument(React.createElement(SignSectionView, {
          model: signview,
          noScreenshot: true
        }));

        TestUtils.Simulate.click(sectionView.signButtonNode());

        window.BrowserInfo = oldBrowserInfo;
      });
    });

    after(function () {
      server.restore();
    });
  });
});
