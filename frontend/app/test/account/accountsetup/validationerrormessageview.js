var React = require("react");

var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var ValidationErrorMessageView = require(
  "../../../scripts/account/setup/validationerrormessageview"
);

describe("account/setup/validationerrormessageview", function () {
  var container = null;

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  var renderComponent = function (children) {
    container = document.createElement("div");

    var component = React.render(
      React.createElement(ValidationErrorMessageView, {}, children), container
    );

    return component;
  };

  it("should render children", function () {
    var component = renderComponent("spam");
    assert.equal(component.getDOMNode().innerText, "spam")
  });
});
