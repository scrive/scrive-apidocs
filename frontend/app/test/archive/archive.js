var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var ArchiveView = require("../../scripts/archive/archive");
var AttachmentsList = require("../../scripts/archive/attachments");
var DocumentsList = require("../../scripts/archive/documents");
var TemplatesList = require("../../scripts/archive/templates");
var TrashList = require("../../scripts/archive/trash");

describe("archive/archive", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || ArchiveView;

    var actualProps = _.extendOwn(
      {
        companyAdmin: false,
        month: 2,
        year: 2017,
        hasDataRetentionPolicy: true,
        immediateTrash: true
      },
      props || {}
    );

    var component = React.render(
      React.createElement(componentClass, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();

    window.location.hash = "";
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.scrolled);
  });

  it("should render the container without scrolled class", function () {
    var component = renderComponent();
    assert.isFalse($(component.getDOMNode()).hasClass("scrolled"));
  });

  it("should render the container with scrolled class", function () {
    var component = renderComponent();
    component.setState({scrolled: true});

    assert.isTrue($(component.getDOMNode()).hasClass("scrolled"));
  });

  it("should render the tabs", function () {
    var component = renderComponent();

    var tabs = $(".tabs", component.getDOMNode());
    assert.lengthOf(tabs, 1);

    assert.equal(
      $("li:nth-child(1)", tabs).text(), localization.archive.documents.name
    );
    assert.equal(
      $("li:nth-child(2)", tabs).text(), localization.archive.templates.name
    );
    assert.equal(
      $("li:nth-child(3)", tabs).text(), localization.archive.attachments.name
    );
    assert.equal(
      $("li:nth-child(4)", tabs).text(), localization.archive.trash.name
    );
  });

  it("should activate the documents tab by default", function () {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(1)", component.getDOMNode());
    assert.isTrue(tab.hasClass("active"));
    assert.equal(window.location.hash, "#documents");
  });

  it("should activate the templates tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#templates");
        done();
      }
    );
  });

  it("should activate the attachments tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#attachments");
        done();
      }
    );
  });

  it("should activate the branded trash tab by clicking on it", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        assert.equal(window.location.hash, "#trash");
        done();
      }
    );
  });

  it("should configure and render the users view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(1)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var documentsView = TestUtils.findRenderedComponentWithType(
          component, DocumentsList
        );

        assert.isFalse(documentsView.props.forCompanyAdmin);
        assert.isFalse(documentsView.props.loadLater);
        assert.equal(documentsView.props.month, 2);
        assert.equal(documentsView.props.year, 2017);
        assert(documentsView.props.hasDataRetentionPolicy);

        done();
      }
    );
  });

  it("should configure and render the templates view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(2)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var templatesView = TestUtils.findRenderedComponentWithType(
          component, TemplatesList
        );

        assert.isFalse(templatesView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the attachments view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(3)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var attachmentsView = TestUtils.findRenderedComponentWithType(
          component, AttachmentsList
        );

        assert.isFalse(attachmentsView.props.loadLater);

        done();
      }
    );
  });

  it("should configure and render the trash view when its tab is active", function (done) {
    var component = renderComponent();

    var tab = $(".tabs li:nth-child(4)", component.getDOMNode());
    TestUtils.Simulate.click(tab[0]);

    util.waitUntil(
      function () {
        return tab.hasClass("active");
      },
      function () {
        var trashView = TestUtils.findRenderedComponentWithType(
          component, TrashList
        );

        assert.isFalse(trashView.props.forCompanyAdmin);
        assert.isFalse(trashView.props.loadLater);
        assert.equal(trashView.props.month, 2);
        assert.equal(trashView.props.year, 2017);
        assert(trashView.props.immediateTrash);

        done();
      }
    );
  });
});
