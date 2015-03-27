/** @jsx React.DOM */

define(["Underscore", "Backbone", "React", "legacy_code"], function (_, Backbone, React) {
  return React.createClass({
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
});
