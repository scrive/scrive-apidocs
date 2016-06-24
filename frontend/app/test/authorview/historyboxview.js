var backend = require("../backend");
var util = require("../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var HistoryBoxView = require("../../scripts/authorview/historyboxview.jsx");

describe("authorview/historyboxview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        HistoryBoxView,
        {
          document: document_,
          onHistoryRefreshSuccess: sinon.spy(),
          onHistoryRefreshError: sinon.spy()
        }
      ),
      $('body')[0]
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      sinon.stub(document_, 'currentViewerIsAuthor').returns(false);
      sinon.stub(document_, 'currentSignatoryCanSign').returns(false);
      sinon.stub(document_, 'isSignedAndClosed').returns(false);
      sinon.stub(document_, 'closed').returns(false);
      sinon.stub(document_, 'isSignedNotClosed').returns(false);
      sinon.stub(document_, 'pending').returns(false);
      sinon.stub(document_, 'canceled').returns(false);
      sinon.stub(document_, 'rejected').returns(false);
      sinon.stub(document_, 'timedout').returns(false);

      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("computes status text for document not signed by author", function (done) {
    document_.currentViewerIsAuthor.returns(true);
    document_.currentSignatoryCanSign.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.signNow);
        done();
      }
    );
  });

  it("computes status text for signed and closed document", function (done) {
    document_.isSignedAndClosed.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.signedAndClosed);
        done();
      }
    );
  });

  it("computes status text for closed document", function (done) {
    document_.closed.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.closed);
        done();
      }
    );
  });

  it("computes status text for signed and open document", function (done) {
    document_.isSignedNotClosed.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.signedNotClosed);
        done();
      }
    );
  });

  it("computes status text for pending document", function (done) {
    document_.pending.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.pending);
        done();
      }
    );
  });

  it("computes status text for canceled document", function (done) {
    document_.canceled.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.canceled);
        done();
      }
    );
  });

  it("computes status text for rejected document", function (done) {
    document_.rejected.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.rejected);
        done();
      }
    );
  });

  it("computes status text for timedout document", function (done) {
    document_.timedout.returns(true);

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), localization.authorview.timeouted);
        done();
      }
    );
  });

  it("computes status text for unknown document status", function (done) {
    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.statusText(), "");
        done();
      }
    );
  });

  it("computes due date description", function (done) {
    sinon.stub(document_, 'timeouttime').returns(new Date(2016, 5, 23));

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(
          component.dueDateDescription(),
          localization.docsignview.dueDate + " " + "2016-06-23"
        );

        done();
      }
    );
  });

  it("computes headline text for pending document", function (done) {
    document_.pending.returns(true);
    sinon.stub(document_, 'timeouttime').returns(new Date(2016, 5, 23));

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(
          component.headlineText(),
          component.statusText() + " - " + localization.docsignview.dueDate + " " + "2016-06-23"
        );

        done();
      }
    );
  });

  it("computes headline text using status text", function (done) {
    component = renderComponent();
    sinon.stub(component, 'statusText').returns('Dummy status text');

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(component.headlineText(), component.statusText());
        done();
      }
    );
  });

  it("proxies the ready state to the underlying component", function (done) {
    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        sinon.stub(component.refs.docHistoryView, 'ready').returns(false);

        assert.isFalse(component.ready());
        assert.isTrue(component.refs.docHistoryView.ready.called);
        done();
      }
    );
  });

  it("refreshes history", function (done) {
    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        // Using stub instead of spy since spies call the underlying function
        // and I don't want that because it triggers rendering of the component.
        sinon.stub(component.refs.docHistoryView, 'checkIfHistoryChangedAndCallback');

        component.refreshHistory();

        assert.isTrue(
          component.refs.docHistoryView.checkIfHistoryChangedAndCallback.calledWith(
            component.props.onHistoryRefreshSuccess, component.props.onHistoryRefreshError
          )
        );

        done();
      }
    );
  });

  it("proxies the expanded property to the underlying component", function (done) {
    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        sinon.stub(component.refs.docHistoryView, 'expanded').returns(true);
        sinon.stub(component.refs.docHistoryView, 'setExpanded');

        assert.isTrue(component.isExpanded());
        assert.isTrue(component.refs.docHistoryView.expanded.called);

        component.setIsExpanded(false);
        assert.isTrue(component.refs.docHistoryView.setExpanded.calledWith(false));

        done();
      }
    );
  });

  it("renders the headline text", function (done) {
    document_.pending.returns(true);
    sinon.stub(document_, 'timeouttime').returns(new Date(2016, 5, 23));

    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.equal(
          $('.headline', React.findDOMNode(component)).text(), component.headlineText()
        );

        done();
      }
    );
  });

  it("renders the history view", function (done) {
    component = renderComponent();

    util.waitUntil(
      function () {
        return component.ready();
      },
      function () {
        assert.isDefined(component.refs.docHistoryView);
        done();
      }
    );
  });
});
