var backend = require("../../backend");
var util = require("../../util");
var sinon = require("sinon/pkg/sinon");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var FilePageView = require("../../../scripts/authorview/fileview/filepageview");

describe("/authorview/fileview/filepageview", function () {
  var server, document_, page;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        FilePageView,
        {page: page, highlightedPages: [], onReady: sinon.spy()}
      ),
      $("body")[0]
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      page = document_.mainfile().pages()[0];

      util.addPlacement(doc, null, 1, {type: "checkbox"});
      util.addPlacement(doc, null, 1, {type: "signature"});
      util.addPlacement(doc, null, 1, {type: "text"});
      util.addPlacement(doc, null, 1, {type: "radiogroup"});

      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should compute the image source URL", function () {
    var expectedSrc = (
      "/pages/" + page.file().fileid() + "/" + page.number() +
      page.file().queryPart({"pixelwidth": page.width()})
    );

    var component = renderComponent();
    assert.equal(component.imageSrc(), expectedSrc);
  });

  it("should return the ready state", function () {
    var component = renderComponent();
    assert.isFalse(component.ready());
  });

  it("should update the ready state when image loads", function () {
    var component = renderComponent();
    sinon.spy(component, 'updateImageSize');
    sinon.spy(component, 'updatePlacementsContainerState');

    component.onImageLoad();

    util.waitUntil(
      function () {
        return component.props.onReady.called;
      },
      function () {
        assert.isTrue(component.ready());
      }
    );
  });

  it("should handle the ready state change", function (done) {
    var component = renderComponent();
    sinon.spy(component, 'updateImageSize');
    sinon.spy(component, 'updatePlacementsContainerState');

    component.onImageLoad();

    util.waitUntil(
      function () {
        return component.props.onReady.called;
      },
      function () {
        assert.isTrue(component.updateImageSize.called);
        assert.isTrue(component.updatePlacementsContainerState.called);
        done();
      }
    );
  });

  it("should calculate the page size according to image size", function (done) {
    sinon.spy(page, "setSize");

    var component = renderComponent();
    var imageEl = $("img", React.findDOMNode(component));
    imageEl.width(320);
    imageEl.height(200);

    component.setState({imageReady: true});

    util.waitUntil(
      function () {
        return component.props.onReady.called;
      },
      function () {
        sinon.assert.called(page.setSize);
        done();
      }
    );
  });

  it("should update placement container's state with page size", function (done) {
    sinon.stub(page, "setSize", function () {
      page.set({width: 320, height: 200}, {silent: true});
    });

    var component = renderComponent();
    component.setState({imageReady: true});

    util.waitUntil(
      function () {
        return component.props.onReady.called;
      },
      function () {
        assert.equal(
          component.refs.placementsContainer.state.pageWidth,
          component.props.page.width()
        );

        assert.equal(
          component.refs.placementsContainer.state.pageHeight,
          component.props.page.height()
        );

        done();
      }
    );
  });

  it("should render the page image", function () {
    var component = renderComponent();
    var imageEl = $("img", React.findDOMNode(component));
    assert.lengthOf(imageEl, 1);

    assert.equal(imageEl.attr("src"), component.imageSrc());
  });

  it("should render the placements container", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.placementsContainer);
  });

  it("should not render the placements container for closed document", function () {
    sinon.stub(document_, "closed").returns(true);

    var component = renderComponent();
    assert.isUndefined(component.refs.placementsContainer);
  });

  it("should render the checkbox placement", function () {
    var compontent = renderComponent();
    assert.lengthOf($(".placedcheckbox", React.findDOMNode(compontent)), 1);
  });

  it("should render the signature placement", function () {
    var compontent = renderComponent();
    assert.lengthOf($(".empty-signature", React.findDOMNode(compontent)), 1);
  });

  it("should render the text placement", function () {
    var compontent = renderComponent();
    assert.lengthOf($(".placedfield-placement-wrapper", React.findDOMNode(compontent)), 1);
  });

  it("should render the radiobutton placement", function () {
    var compontent = renderComponent();
    assert.lengthOf($(".placedradiobutton", React.findDOMNode(compontent)), 1);
  });
});
