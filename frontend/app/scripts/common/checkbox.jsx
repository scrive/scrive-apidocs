var React = require("react");
var classNames = require("classnames");

/**
 * A checkbox component. Used in branding settings.
 *
 * Properties taken:
 * label, a string labeling the checkbox.
 * checked, a boolean representing if checked checked(=true)/unchecked(=false) state.
 * onChange, a callback function being called every time the checkbox is clicked.
 *
 * Example usage:
 * var checkbox = React.render(React.createElement(Checkbox.Checkbox,{
 *    checked: args.checked,
 *    label: args.label,
 *    onChange : args.function, Bool -> ()
 * }), div);
 *
 */

  module.exports = React.createClass({
    propTypes: {
      label: React.PropTypes.string.isRequired,
      checked: React.PropTypes.bool.isRequired,
      onChange: React.PropTypes.func
    },
    handleClick: function() {
      // The mixpanel events are named "Check|Uncheck <label>"
      var eventName = (this.props.checked ? 'Uncheck' : 'Check') + ' ' + this.props.label;
      mixpanel.track(eventName);
      if (this.props.onChange) { this.props.onChange(!this.props.checked); }

    },

    render: function() {

      var checkboxClasses = classNames({
        'checkbox': true,
        'checked': this.props.checked
      });

      return (
        <div className={classNames("checkbox-box",this.props.className)} onClick={this.handleClick}>
          <div className={checkboxClasses} style={this.props.style} tabIndex="0" onKeyDown={this.handleKeyDn}>
            <div className="checkmark" />
          </div>
          { this.props.label &&
            <label>{this.props.label}</label>
          }
        </div>
      );
    }
  });
