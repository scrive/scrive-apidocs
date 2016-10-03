var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var Document = require("../../../js/documents.js").Document;
var FileView = require("../../../scripts/authorview/fileview/fileview");

describe("/authorview/fileview/fileview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        FileView,
        {model: document_, onReady: sinon.spy()}
      ),
      $("body")[0]
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    // Can't use util.createDocument() here as the method calls fetch()
    // on doc.mainfile() ultimately causing the tests to go crazy.
    document_ = new Document({id: 0});
    document_.fetch({processData: true, cache: false});
    document_.on("change", function () {
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should render placeholder when the model isn't ready", function (done) {
    sinon.stub(document_.mainfile(), 'ready').returns(false);

    var component = renderComponent();
    var componentNode = React.findDOMNode(component);

    util.waitUntil(
      function () {
        return component.props.model.mainfile().pages().length > 0;
      },
      function () {
        assert.lengthOf($(".waiting4page", componentNode), 1);
        assert.lengthOf($(".pagediv", componentNode), 0);

        done();
      }
    );
  });

  it("should render document page when the model is ready", function (done) {
    var component = renderComponent();
    var componentNode = React.findDOMNode(component);

    util.waitUntil(
      function () {
        return component.props.model.mainfile().ready();
      },
      function () {
        assert.lengthOf($(".waiting4page", componentNode), 0);
        assert.lengthOf($(".pagediv", componentNode), 1);

        done();
      }
    );
  });

  it("should set ready state to false when model isn't ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.props.model.mainfile().ready();
      },
      function () {
        sinon.stub(document_.mainfile(), 'ready').returns(false);

        component.refreshReady();
        assert.isFalse(component.ready());

        done();
      }
    );
  });

  it("should set ready state to false when pages aren't ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.props.model.mainfile().ready();
      },
      function () {
        _.each(component._pageRefs, function(item, index) {
          sinon.stub(item, 'ready').returns(false);
        });

        component.refreshReady();
        assert.isFalse(component.ready());

        done();
      }
    );
  });

  it("should set ready state to true when model and pages are ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.props.model.mainfile().ready();
      },
      function () {
        _.each(component._pageRefs, function(item, index) {
          sinon.stub(item, 'ready').returns(true);
        });

        component.refreshReady();
        assert.isTrue(component.ready());

        done();
      }
    );
  });

  it("should call onReady when model and pages are ready", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.props.model.mainfile().ready();
      },
      function () {
        _.each(component._pageRefs, function(item, index) {
          sinon.stub(item, 'ready').returns(true);
        });

        component.refreshReady();
        assert.isTrue(component.props.onReady.called);

        done();
      }
    );
  });
});
