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

        var container = TestUtils.renderIntoDocument(util.taskContextContainer(SignSectionView, {
          model: signview,
          noScreenshot: true
        }));

        var sectionView = container.refs.comp;

        TestUtils.Simulate.click(sectionView.signButtonNode());
        TestUtils.Simulate.click(sectionView.refs.rejectButton.getDOMNode());
      });

      it("should test component in small screen", function (done) {
        util.withSmallScreen(function () {
          var signview = new SignView();
          signview.setDocument(doc);

          var container = TestUtils.renderIntoDocument(util.taskContextContainer(SignSectionView, {
            model: signview,
            noScreenshot: true
          }));

          var sectionView = container.refs.comp;

          TestUtils.Simulate.click(sectionView.signButtonNode());

          done();
        });
      });
    });

    after(function () {
      server.restore();
    });
  });
});
