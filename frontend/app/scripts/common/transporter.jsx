import $ from "jquery";
import React from "react";

module.exports = React.createClass({
  mixins: [React.addons.PureRenderMixin],

  propTypes: {
    node: React.PropTypes.instanceOf($).isRequired
  },

  componentWillMount: function () {
    this._portal = $("<div/>");
    if (this.props.node) {
      this.props.node.append(this._portal);
    }
  },

  renderPortal: function () {
    return <div>{this.props.children}</div>;
  },

  componentDidMount: function () {
    React.render(this.renderPortal(), this._portal[0]);
  },


  componentWillUpdate: function (nextProps) {
    if (nextProps.node !== this.props.node) {
      if (this.props.node && this._portal) {
        this._portal.detach();
      }

      if (nextProps.node) {
        nextProps.node.append(this._portal);
      }
    }
  },

  componentDidUpdate: function () {
    React.render(this.renderPortal(), this._portal[0]);
  },

  componentWillUnmount: function () {
    React.unmountComponentAtNode(this._portal[0]);
    if (this.props.node && this._portal) {
      this._portal.detach();
    }
  },

  render: function () {
    return null;
  }
});
