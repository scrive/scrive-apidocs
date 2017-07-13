// This section is left commented out. It's used for debugging weird bugs
// in tests that are caused by not cleaning up rendered components.
/*var React = require("react");

var origRender = React.render;
sinon.stub(React, "render", function () {
  var result = origRender.apply(this, arguments);
  var node = result.getDOMNode()

  if (node) {
    console.warn('Rendered!', result.constructor.name, result.getDOMNode().dataset.reactid);
  } else {
    console.warn('Rendered!', result.constructor.name);
  }

  return result;
});*/

// Entry point for all tests
var context = require.context(".", true, /\.\/.+\/.+\.js$/);
context.keys().forEach(context);
module.exports = context;
