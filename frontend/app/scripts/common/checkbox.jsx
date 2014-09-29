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
 * var checkbox = React.renderComponent(Checkbox({
 *    initiallyChecked: args.checked,
 *    label: args.label
 * }), div);
 *
 */

define(['React'], function(React) {
  return React.createClass({
    propTypes: {
      label: React.PropTypes.string.isRequired,
      initiallyChecked: React.PropTypes.bool.isRequired,
      onChange: React.PropTypes.func,
      style: React.PropTypes.object
    },

    getInitialState: function() {
      return {checked: this.props.initiallyChecked};
    },

    handleKeyDn: function(ev) {
      if (ev.key == " ") { this.handleClick(); ev.preventDefault(); }
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
          <div className={classes} style={this.props.style} tabIndex="0" onKeyDown={this.handleKeyDn}>
            <div className="checkmark" />
          </div>
          { this.props.label &&
            <label>{this.props.label}</label>
          }
        </div>
      );
    }
  });
});

