var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var SignaturePlacementView = require("../../../scripts/authorview/fileview/signatureplacementview");
var Field = require("../../../js/fields.js").Field;

describe("/authorview/fileview/signatureplacementview", function () {
  var document_, field;

  var renderComponent = function () {
    var parent = $("<div />");
    parent.appendTo($("body"));

    var component = React.render(
      React.createElement(
        SignaturePlacementView,
        {
          model: field.placements()[0],
          pageWidth: 320,
          pageHeight: 200
        }
      ),
      parent[0]
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      field = new Field({
        signatory: document_.signatories()[0],
        placements: [
          {
            placed: true,
            fsrel: 0.5,
            hrel: 0.25,
            wrel: 0.1,
            xrel: 0.1,
            yrel: 0.25
          }
        ]
      });

      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should compute the field style without signature file", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    assert.equal(computedStyle.display, "none");
    assert.equal(computedStyle.height, 50);
    assert.equal(computedStyle.left, 32);
    assert.equal(computedStyle.top, 50);
    assert.equal(computedStyle.width, 32);
  });

  it("should compute the field style with signature file", function () {
    field.set({signature: "42"}, {silent: true});

    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    assert.equal(computedStyle.display, undefined);
  });

  it("should compute the image source URL", function () {
    field.set("signature", "42")
    var component = renderComponent();

    var expectedSrc = (
      "/api/frontend/documents/" + document_.documentid() + "/files/" +
      "42" + "/image.png" +
      "?signatory_id=" + document_.currentSignatory().signatoryid()
    );

    assert.equal(component.imageSrc(), expectedSrc);
  });

  it("should compute the image style", function () {
    var component = renderComponent();

    var imageStyle = component.imageStyle();
    assert.equal(imageStyle.height, 50);
    assert.equal(imageStyle.width, 32);
  });

  it("should assign the field style", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    var componentNode = React.findDOMNode(component);

    assert.equal(componentNode.style["display"], computedStyle.display);
    assert.equal(componentNode.style["height"], computedStyle.height + "px");
    assert.equal(componentNode.style["left"], computedStyle.left + "px");
    assert.equal(componentNode.style["top"], computedStyle.top + "px");
    assert.equal(componentNode.style["width"], computedStyle.width + "px");
  });

  it("should add empty-signature class to field without signature", function () {
    var component = renderComponent();
    assert.isTrue($(React.findDOMNode(component)).hasClass("empty-signature"));
  });

  it("should not render the image in field without signature", function () {
    var component = renderComponent();
    assert.lengthOf($("img", React.findDOMNode(component)), 0);
  });

  it("should properly render the image in field with signature", function () {
    field.set({signature: "42"}, {silent: true});

    var component = renderComponent();
    var imageEl = $("img", React.findDOMNode(component));
    assert.lengthOf(imageEl, 1);

    assert.equal(imageEl.attr("src"), component.imageSrc());

    var imageStyle = component.imageStyle();
    assert.equal(imageEl.attr("height"), imageStyle.height);
    assert.equal(imageEl.css("height"), imageStyle.height + "px");
    assert.equal(imageEl.attr("width"), imageStyle.width);
    assert.equal(imageEl.css("width"), imageStyle.width + "px");
  });
});
