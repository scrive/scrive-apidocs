var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");

  module.exports = React.createClass({
    getInitialState: function () {
      return {
        open: false
      };
    },

    open: function () {
      this.setState({
        open: true
      });
    },

    render: function () {
      return (
        <div {...this.props}>
          {/* if */ !this.state.open &&
            <a className="fieldTypeSetter-more" onClick={this.open}>
              {localization.designview.moreSettings}
            </a>
          }
          {/* else */ this.state.open &&
            this.props.children
          }
        </div>
      );
    }
  });
