var Backbone = require("backbone");
var React = require("react");
var $ = require("jquery");
  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    injectArrows: function () {
      if (!this.isMounted()) { return ; }

      var model = this.props.model;
      var $node = $(this.getDOMNode());

      if (model.hasArrows() && model.get("arrow") === undefined) {
        $node.empty();
        $node.append(model.arrow().view().el);
      }
    },

    componentDidMount: function () {
      this.injectArrows();
    },

    render: function () {
      return <div />;
    }
  });
