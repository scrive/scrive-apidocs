/** @jsx React.DOM */

/**
 * A checkbox component. Used in branding settings.
 *
 * Properties taken:
 * label, a string labeling the checkbox.
 * initiallyChecked, a boolean representing the initial checked(=true)/unchecked(=false) state.
 * onChange, a callback function being called every time the checkbox is checked or unchecked.
 *
 * Example usage: 
 * var checkbox = React.renderComponent(Checkbox.Checkbox({
 *    initiallyChecked: args.checked,
 *    label: args.label
 * }), div);
 *
 */

define(['React'], function(React) {
  var expose = {};

  var Checkbox = React.createClass({
    propTypes: {
      label: React.PropTypes.string.isRequired,
      initiallyChecked: React.PropTypes.bool.isRequired,
      onChange: React.PropTypes.func
    },

    getInitialState: function() {
      return {checked: this.props.initiallyChecked};
    },

    handleClick: function() {
      // The mixpanel events are named "Check|Uncheck <label>"
      var eventName = (this.state.checked ? 'Uncheck' : 'Check') + ' ' + this.props.label;
      mixpanel.track(eventName);

      var newState = !this.state.checked;

      if (this.props.onChange) { this.props.onChange(newState); }

      this.setState({'checked': newState});
    },

    render: function() {
      var classSet = React.addons.classSet;
      var classes = classSet({
        'checkbox': true,
        'checked': this.state.checked
      });

      return (
        <div className="checkbox-box" onClick={this.handleClick}>
          <div className={classes} />
          <label>{this.props.label}</label>
        </div>
      );
    }
  });

  expose.Checkbox = Checkbox;

  return expose;
});

