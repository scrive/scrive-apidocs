var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var FileView = require("../../../scripts/designview/fileview/fileview.jsx");

describe("designview/fileview", function () {
  var server, document_, mainfile;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        FileView,
        {model: mainfile, pixelWidth: 200, removePageFunc: function() {}}
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
      mainfile = doc.mainfile();
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should show coordinate axes", function () {
    var component = renderComponent();
    component.showCoordinateAxes();

    assert.isTrue(component.state.visibleCoordinates);
  });

  it("should hide coordinate axes", function () {
    var component = renderComponent();
    component.hideCoordinateAxes();

    assert.isFalse(component.state.visibleCoordinates);
  });

  it("should set ready state to false when model isn't ready", function () {
    sinon.stub(mainfile, "ready").returns(false);

    var component = renderComponent();
    assert.isFalse(component.ready());
  });

  it("should set ready state to false when model has no pages", function () {
    sinon.stub(mainfile, "ready").returns(true);
    sinon.stub(mainfile, "pages").returns([]);

    var component = renderComponent();
    assert.isFalse(component.ready());
  });

  it("should set ready to false when some pages are missing", function () {
    sinon.stub(mainfile, "ready").returns(true);

    var component = renderComponent();
    component.setState({images: []});

    assert.isFalse(component.ready());
  });

  it("should set ready to false when some pages aren't laoded", function () {
    sinon.stub(mainfile, "ready").returns(true);

    var component = renderComponent();

    var dummyImg = new Image();
    dummyImg.src = 'http://localhost/idontexist.jpg';
    dummyImg.complete = false;

    component.setState({images: [dummyImg]});
    assert.isFalse(component.ready());
  });

  it("should set ready state to true", function () {
    var component = renderComponent();

    var dummyImg = new Image();
    dummyImg.complete = true;

    component.setState({images: [dummyImg]});
    assert.isTrue(component.ready());
  });

  it("should set first page ready state to false when there's no images", function () {
    var component = renderComponent();
    component.setState({images: []});

    assert.isFalse(component.readyFirstPage());
  });

  it("should set first page ready state to false when the image isn't loaded", function () {
    var component = renderComponent();

    var dummyImg = new Image();
    dummyImg.src = 'http://localhost/idontexist.jpg';
    dummyImg.complete = false;

    component.setState({images: [dummyImg]});
    assert.isFalse(component.readyFirstPage());
  });

  it("should set first page ready state to true", function () {
    var component = renderComponent();

    var dummyImg = new Image();
    dummyImg.complete = true;

    component.setState({images: [dummyImg]});
    assert.isTrue(component.readyFirstPage());
  });

  it("should update images", function () {
    var component = renderComponent();
    component.updateImages();

    assert.lengthOf(component.state.images, mainfile.pages().length);

    var firstPage = mainfile.pages()[0];
    var firstPageImage = component.state.images[0];
    assert.include(
      firstPageImage.src,
      "/pages/" + mainfile.fileid() + "/" + firstPage.number() + "?pixelwidth=200"
    );
  });

  it("should handle image load", function () {
    sinon.spy(mainfile, "trigger");

    var component = renderComponent();
    sinon.stub(component, "ready").returns(true);

    component.handleLoad(0);

    assert.isTrue(mainfile.trigger.calledWith("FirstPageReady"));
    assert.isTrue(mainfile.trigger.calledWith("view:ready"));
    assert.isTrue(mainfile.trigger.calledWith("change"));
  });

  it("should render a placeholder when the file isn't ready", function () {
    sinon.stub(mainfile, "ready").returns(false);

    var component = renderComponent();

    assert.lengthOf($(".waiting4page"), 1);
    assert.lengthOf($(".pagediv"), 0);
  });

  it("should render page view when the file is ready and image is present", function () {
    var component = renderComponent();

    var dummyImg = new Image();
    dummyImg.complete = true;

    component.setState({images: [dummyImg]})

    assert.lengthOf($(".waiting4page"), 0);
    assert.lengthOf($(".pagediv"), 1);
  });

  it("should not render coordinate lines if they're hidden", function () {
    var component = renderComponent();
    component.hideCoordinateAxes();

    assert.lengthOf($(".vline"), 0);
    assert.lengthOf($(".hline"), 0);
  });

  it("should render coordinate lines if they're shown", function () {
    var component = renderComponent();
    component.showCoordinateAxes();

    assert.lengthOf($(".vline"), 1);
    assert.lengthOf($(".hline"), 1);
  });
});