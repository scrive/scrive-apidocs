/** @jsx React.DOM */

/*
  Transitory mixin that mimics the behavior of the old type setters.
  This should be removed in the future when the rest of fieldplacementview
  has been rewritten
*/

define(["jquery", "React", "legacy_code"], function($, React) {
  var SelfUnmountMixin = {
    getInitialState: function () {
      return {
        container: null
      };
    }

    , componentDidMount: function () {
      var parent = $(this.getDOMNode()).parent();
      this.setState({
        container: parent[0]
      });
    }

    , componentWillUnmount: function () {
      $(this.state.container).remove();
    }

    , unmount: function () {
      React.unmountComponentAtNode(this.state.container);
    }
  };

  return SelfUnmountMixin;
});
