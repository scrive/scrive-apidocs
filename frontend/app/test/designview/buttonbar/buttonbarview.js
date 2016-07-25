var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var ButtonBarView = require("../../../scripts/designview/buttonbar/buttonbarview.jsx");

describe("designview/buttonbarview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        ButtonBarView,
        {document: document_}
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
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should render buttons for template with file", function () {
    sinon.stub(document_, 'isTemplate').returns(true);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 2);
    assert.lengthOf($('.button-save-template', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-save-draft', React.findDOMNode(component)), 0);
    assert.lengthOf($('.button-save-as-template', React.findDOMNode(component)), 0);
    assert.lengthOf($('.sendButton', React.findDOMNode(component)), 0);
  });

  it("should render buttons for template without file", function () {
    sinon.stub(document_, 'isTemplate').returns(true);
    sinon.stub(document_, 'mainfile').returns(null);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 0);
  });

  it("should render buttons for document with file", function () {
    sinon.stub(document_, 'isTemplate').returns(false);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 4);
    assert.lengthOf($('.button-save-template', React.findDOMNode(component)), 0);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-save-draft', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-save-as-template', React.findDOMNode(component)), 1);
    assert.lengthOf($('.sendButton', React.findDOMNode(component)), 1);
  });

  it("should render buttons for document without file", function () {
    sinon.stub(document_, 'isTemplate').returns(false);
    sinon.stub(document_, 'mainfile').returns(null);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 3);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 0);
  });

  it("should disable the send button if document isn't ready", function () {
    sinon.stub(document_, 'ready').returns(false);

    var component = renderComponent();

    assert.isTrue($('.sendButton', React.findDOMNode(component)).hasClass('disabled'));
  });

  it("should disable the send button if document has problems", function () {
    sinon.stub(document_, 'hasProblems').returns(true);

    var component = renderComponent();

    assert.isTrue($('.sendButton', React.findDOMNode(component)).hasClass('disabled'));
  });

  it("should enable the send button if document is OK", function () {
    sinon.stub(document_, 'ready').returns(true);
    sinon.stub(document_, 'hasProblems').returns(false);

    var component = renderComponent();

    assert.isFalse($('.sendButton', React.findDOMNode(component)).hasClass('disabled'));
  });
});