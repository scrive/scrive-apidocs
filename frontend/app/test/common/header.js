var React = require("react");
var Header = require("../../scripts/pages/header");
var User = require("../../js/account/user.js").User;
var $ = require("jquery");

var TestUtils = React.addons.TestUtils;

var defaultProps = { archive: false, account: false };

describe("common/header", function () {
  var currentUserStub;
  var userReadyStub;
  var appfrontendStub;

  before(function () {
    appfrontendStub = sinon.stub();
    userReadyStub = sinon.stub().returns(true);
    currentUserStub = sinon.stub(User, "currentUser", function () {
      return {
        company: function () {
          return { appfrontend: appfrontendStub };
        },
        ready: userReadyStub,
        reload: function () { }
      };
    });
  });

  after(function () {
    currentUserStub.restore();
  });

  it("should contain link to new frontend when appfrontend is enabled", function () {
    appfrontendStub.returns(true);
    var header = TestUtils.renderIntoDocument(React.createElement(Header, defaultProps));

    assert.lengthOf($("#switch-frontend", React.findDOMNode(header)), 1);
  });

  it("should not contain link to new frontend when appfrontend is disabled", function () {
    appfrontendStub.returns(false);
    var header = TestUtils.renderIntoDocument(React.createElement(Header, defaultProps));

    assert.lengthOf($("#switch-frontend", React.findDOMNode(header)), 0);
  });

  it("should not contain link to new frontend when user is not loaded yet", function () {
    userReadyStub.returns(false);
    var header = TestUtils.renderIntoDocument(React.createElement(Header, defaultProps));

    assert.lengthOf($("#switch-frontend", React.findDOMNode(header)), 0);
  });
});