var React = require("react");
var _ = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var DesignSignatoryAttachment = require(
  "../../../scripts/designview/signatoryattachments/designsignatoryattachment"
);
var SignatoryAttachmentRow = require(
  "../../../scripts/designview/signatoryattachments/signatoryattachmentrow"
);
var SignatoryAttachmentsTable = require(
  "../../../scripts/designview/signatoryattachments/signatoryattachmentstable"
);
var Signatory = require("../../../js/signatories.js").Signatory;

describe("designview/signatoryattachments/signatoryattachmentstable", function () {
  var container = null;
  var document_ = null;
  var attachments = null;
  var signatory = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var defaultProps = {
      attachments: attachments,
      signatories: document_.signatories(),
      onRemove: sinon.stub()
    };

    var actualProps = _.extendOwn({}, defaultProps, props || {});

    var component = React.render(
      React.createElement(SignatoryAttachmentsTable, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      signatory = document_.signatories()[1];

      attachments = [
        new DesignSignatoryAttachment({
          name: "spam",
          description: "eggs",
          signatory: signatory,
          isRequired: true
        }),
        new DesignSignatoryAttachment({
          name: "eggs",
          description: "spam",
          signatory: signatory,
          isRequired: false
        })
      ];

      done();
    });
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
  });

  it("should render a row for every attachment", function () {
    var component = renderComponent();

    var rows = TestUtils.scryRenderedComponentsWithType(
      component, SignatoryAttachmentRow
    );

    assert.equal(rows.length, 2);

    assert.equal(rows[0].props.attachment, attachments[0]);
    assert.equal(rows[0].props.signatories, component.props.signatories);
    assert.equal(rows[0].props.onRemove, component.props.onRemove);
  });
});
