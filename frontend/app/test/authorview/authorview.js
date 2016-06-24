var backend = require("../backend");
var util = require("../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var AuthorAttachment = require("../../js/authorattachment.js").AuthorAttachment;
var SignatoryAttachment = require("../../js/signatoryattachment.js").SignatoryAttachment;
var Document = require("../../js/documents.js").Document;
var Signatory = require("../../js/signatories.js").Signatory;

var AuthorView = require("../../scripts/authorview/authorview.jsx");

describe("authorview/authorview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        AuthorView,
        {
          document: document_,
          onReload: sinon.spy()
        }
      ),
      $('body')[0]
    );

    sinon.stub(component, 'scheduleHistoryRefresh');
    sinon.stub(component, 'clearHistoryRefreshTimeout');

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function () {
    // Can't use util.createDocument() here as the method calls fetch()
    // on the document wreaking all kinds of havoc in these tests.
    document_ = new Document({id: 0});
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should fetch the document when it mounts", function () {
    sinon.stub(document_, 'fetch');

    var component = renderComponent();
    assert.isTrue(document_.fetch.calledWith({processData: true, cache: false}));
  });

  it("should initialize the history refresh params when it mounts", function () {
    sinon.stub(document_, 'fetch');

    var component = renderComponent();
    assert.equal(component._historyRefreshCounter, 1);
    assert.isNull(component._historyRefreshTimeout);
    assert.isFalse(component._shouldRefreshHistory);
  });

  it("should clear history refresh timeout when it unmounts", function () {
    sinon.stub(document_, 'fetch');

    var component = renderComponent();

    React.unmountComponentAtNode(React.findDOMNode(component));

    util.waitUntil(
      function () {
        return component.isMounted() == false;
      },
      function () {
        assert.isTrue(component.clearHistoryRefreshTimeout.called);
      }
    );
  });

  it("should trigger clean reload", function () {
    sinon.stub(document_, 'fetch');

    var component = renderComponent();
    component.triggerSilentReloadIfPossible();

    assert.isTrue(component.props.onReload.calledWith(false));
  });

  it("should trigger dirty reload", function () {
    sinon.stub(document_, 'fetch');

    var component = renderComponent();
    component.triggerReload();

    assert.isTrue(component.props.onReload.calledWith(true));
  });

  it("should not refresh history when it isn't mounted", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'refreshHistory');
        sinon.stub(component, 'isMounted').returns(false);

        component.onHistoryRefreshTimeout();
        assert.isFalse(component.refs.historyBoxView.refreshHistory.called);

        done();
      }
    );
  });

  it("should not refresh history when sentinel is false", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'refreshHistory');
        component._shouldRefreshHistory = false;

        component.onHistoryRefreshTimeout();
        assert.isFalse(component.refs.historyBoxView.refreshHistory.called);

        done();
      }
    );
  });

  it("should refresh history", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'refreshHistory');
        component._shouldRefreshHistory = true;

        component.onHistoryRefreshTimeout();

        assert.isTrue(component.refs.historyBoxView.refreshHistory.called);

        done();
      }
    );
  });

  it("should update history refresh counter after refreshing the history", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'refreshHistory');
        component._shouldRefreshHistory = true;

        component.onHistoryRefreshTimeout();

        assert.equal(component._historyRefreshCounter, 3)

        done();
      }
    );
  });

  it("should not update history refresh counter if it greater than 30", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'refreshHistory');
        component._shouldRefreshHistory = true;
        component._historyRefreshCounter = 31

        component.onHistoryRefreshTimeout();

        assert.equal(component._historyRefreshCounter, 31)

        done();
      }
    );
  });

  it("should schedule next refresh after refreshing the history", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'refreshHistory');
        component._shouldRefreshHistory = true;
        component._historyRefreshCounter = 31

        component.onHistoryRefreshTimeout();

        assert.isTrue(component.scheduleHistoryRefresh.called);

        done();
      }
    );
  });

  it("should clear history refresh timeout if it's set", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(window, 'clearTimeout');
        component._historyRefreshTimeout = 1;
        component.clearHistoryRefreshTimeout.restore();

        component.clearHistoryRefreshTimeout();

        assert.isTrue(window.clearTimeout.calledWith(1));

        window.clearTimeout.restore();

        done();
      }
    );
  });

  it("should not clear history refresh timeout if isn't set", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(window, 'clearTimeout');
        component._historyRefreshTimeout = null;
        component.clearHistoryRefreshTimeout.restore();

        component.clearHistoryRefreshTimeout();

        assert.isFalse(window.clearTimeout.called);

        window.clearTimeout.restore();

        done();
      }
    );
  });

  it("should not set new history refresh timeout if sentinel flag is false", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(window, 'setTimeout');
        sinon.stub(component, 'ready').returns(true);
        component._shouldRefreshHistory = false;
        component.scheduleHistoryRefresh.restore();

        component.scheduleHistoryRefresh();

        assert.isFalse(window.setTimeout.called);

        window.setTimeout.restore();

        done();
      }
    );
  });

  it("should set new history refresh timeout even if authorview isn't ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(window, 'setTimeout');
        sinon.stub(component, 'ready').returns(false);
        component._shouldRefreshHistory = true;
        component.scheduleHistoryRefresh.restore();

        component.scheduleHistoryRefresh();

        assert.isTrue(window.setTimeout.called);

        window.setTimeout.restore();

        done();
      }
    );
  });

  it("should set new history refresh timeout", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(window, 'setTimeout').returns(1);
        sinon.stub(component, 'ready').returns(true);
        component._shouldRefreshHistory = true;
        component.scheduleHistoryRefresh.restore();

        component.scheduleHistoryRefresh();

        assert.isTrue(window.setTimeout.calledWith(
          component.onHistoryRefreshTimeout, 1000
        ));

        assert.equal(component._historyRefreshTimeout, 1);

        window.setTimeout.restore();

        done();
      }
    );
  });

  it("stop refreshing history", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        component.stopRefreshingHistory();
        assert.isFalse(component._shouldRefreshHistory);
        assert.isTrue(component.clearHistoryRefreshTimeout.called);

        done();
      }
    );
  });

  it("should proxy current signatory property to subcomponent", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.signatoriesView, 'currentIndex').returns(0);
        sinon.stub(component.refs.signatoriesView, 'setCurrentIndex');

        assert.equal(component.currentSignatoryIndex(), 0);
        assert.isTrue(component.refs.signatoriesView.currentIndex.called);

        component.setCurrentSignatoryIndex(1);
        assert.isTrue(component.refs.signatoriesView.setCurrentIndex.calledWith(1));
        
        done();
      }
    );
  });

  it("should proxy expanded history accessors to subcomponent", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(component.refs.historyBoxView, 'isExpanded').returns(false);
        sinon.stub(component.refs.historyBoxView, 'setIsExpanded');

        assert.equal(component.isHistoryViewExpanded(), false);
        assert.isTrue(component.refs.historyBoxView.isExpanded.called);

        component.setIsHistoryViewExpanded(true);
        assert.isTrue(component.refs.historyBoxView.setIsExpanded.calledWith(true));
        
        done();
      }
    );
  });

  it("should schedule history refresh when the file view is ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        component.onFileViewReady();
        assert.isTrue(component._shouldRefreshHistory);
        assert.isTrue(component.scheduleHistoryRefresh.called);
        
        done();
      }
    );
  });

  it("should stop refreshing history after refresh error", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        component.onHistoryRefreshError();
        assert.isFalse(component._shouldRefreshHistory);
        
        done();
      }
    );
  });

  it("should not be ready when the document isn't ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(document_, 'ready').returns(false);
        
        assert.isFalse(component.ready());
        
        done();
      }
    );
  });

  it("should not be ready if the document needs recall", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(document_, 'ready').returns(true);
        sinon.stub(document_, 'needRecall').returns(true);
        
        assert.isFalse(component.ready());
        
        done();
      }
    );
  });

  it("should not be ready if history box view isn't ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(document_, 'ready').returns(true);
        sinon.stub(document_, 'needRecall').returns(false);
        sinon.stub(component.refs.historyBoxView, 'ready').returns(false);
        
        assert.isFalse(component.ready());
        
        done();
      }
    );
  });

  it("should not be ready if file view isn't ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(document_, 'ready').returns(true);
        sinon.stub(document_, 'needRecall').returns(false);
        sinon.stub(component.refs.historyBoxView, 'ready').returns(true);
        sinon.stub(component.refs.fileView, 'ready').returns(false);
        
        assert.isFalse(component.ready());
        
        done();
      }
    );
  });

  it("should not be ready if evidence attachments view isn't ready", function (done) {
    sinon.stub(document_, "closed").returns(true);
    sinon.stub(document_, "mainfile", function () {
      return document_.file();
    });

    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(document_, 'ready').returns(true);
        sinon.stub(document_, 'needRecall').returns(false);
        sinon.stub(component.refs.historyBoxView, 'ready').returns(true);
        sinon.stub(component.refs.fileView, 'ready').returns(true);
        sinon.stub(component.refs.evidenceAttachmentsView, 'ready').returns(false);
        
        assert.isFalse(component.ready());
        
        done();
      }
    );
  });

  it("should be ready if all conditions are met", function (done) {
    sinon.stub(document_, "closed").returns(true);
    sinon.stub(document_, "mainfile", function () {
      return document_.file();
    });

    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        sinon.stub(document_, 'ready').returns(true);
        sinon.stub(document_, 'needRecall').returns(false);
        sinon.stub(component.refs.historyBoxView, 'ready').returns(true);
        sinon.stub(component.refs.fileView, 'ready').returns(true);
        sinon.stub(component.refs.evidenceAttachmentsView, 'ready').returns(true);
        
        assert.isTrue(component.ready());
        
        done();
      }
    );
  });

  it("should render default subcomponents", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        var componentNode = React.findDOMNode(component);

        assert.lengthOf($('.titlebox', componentNode), 1);
        assert.lengthOf($('.history-box', componentNode), 1);
        assert.lengthOf($('.signatories', componentNode), 1);
        assert.lengthOf($('.document-pages', componentNode), 1);
        assert.lengthOf($('.authorattachments', componentNode), 0);
        assert.lengthOf($('.signatoryattachments', componentNode), 0);
        assert.lengthOf($('.s-evidenceattachments', componentNode), 0);
        
        done();
      }
    );
  });

  it("should render author attachments view if present", function (done) {
    sinon.stub(document_, "authorattachments").returns([
      new AuthorAttachment({
        name: "Test 1",
        required: false,
        add_to_sealed_file: false,
        file_id: "58",
        document: document_
      }),
      new AuthorAttachment({
        name: "Test 2",
        required: true,
        add_to_sealed_file: false,
        file_id: "59",
        document: document_
      })
    ]);

    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        var componentNode = React.findDOMNode(component);
        assert.lengthOf($('.authorattachments', componentNode), 1);
        
        done();
      }
    );
  });

  it("should render signatory attachments view if present", function (done) {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: new Signatory({name: "Test Signatory"})
      })
    ]);

    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        var componentNode = React.findDOMNode(component);
        assert.lengthOf($('.signatoryattachments', componentNode), 1);
        
        done();
      }
    );
  });

  it("should render evidence attachments view if present", function (done) {
    sinon.stub(document_, "closed").returns(true);
    sinon.stub(document_, "mainfile", function () {
      return document_.file();
    });

    var component = renderComponent();

    util.waitUntil(
      function () {
        return document_.ready();
      },
      function () {
        util.waitUntil(
          function () {
            return component.refs.evidenceAttachmentsView.ready();
          }, function () {
            var componentNode = React.findDOMNode(component);
            assert.lengthOf($('.s-evidenceattachments', componentNode), 1);
            
            done();
          }
        )
      }
    );
  });
});
