var React = require("react");
var classNames = require("classnames");
var Track = require("./track");

/**
 * A checkbox component. Used in branding settings.
 *
 * Properties taken:
 * label, a string labeling the checkbox.
 * checked, a boolean representing if checked checked(=true)/unchecked(=false) state.
 * onChange, a callback function being called every time the checkbox is clicked.
 * customEventName, if you prefer 'Check <customEventName>' to 'Check <labelText>'
 *
 * Example usage:
 * var checkbox = React.render(React.createElement(Checkbox.Checkbox,{
 *    checked: args.checked,
 *    label: args.label,
 *    customEventName: "Custom event name",
 *    onChange : args.function, Bool -> ()
 * }), div);
 *
 */

  module.exports = React.createClass({
    propTypes: {
      label: React.PropTypes.string.isRequired,
      checked: React.PropTypes.bool.isRequired,
      disabled: React.PropTypes.bool,
      customEventName: React.PropTypes.bool,
      onChange: React.PropTypes.func
    },

    getDefaultProps : function() {
      return {
        "disabled" : false
      };
    },

    handleClick: function() {
      // The mixpanel events are named "Check|Uncheck <label>"
      var eventName = (this.props.checked ? 'Uncheck' : 'Check') + ' ';
      eventName += (this.props.customEventName !== undefined) ? this.props.customEventName : this.props.label;
      Track.track(eventName);
      if (!this.props.disabled && this.props.onChange) {
        this.props.onChange(!this.props.checked);
      }

    },

    render: function() {

      var checkboxClasses = classNames({
        "checkbox": true,
        "checked": this.props.checked,
        "disabled": this.props.disabled
      });

      return (
        <div className={classNames("checkbox-box",this.props.className)} onClick={this.handleClick}>
          <div className="checkbox-box-wrapper">
            <div className={checkboxClasses} style={this.props.style} tabIndex="0" onKeyDown={this.handleKeyDn}>
              <div className="checkmark" />
            </div>
          </div>
          { this.props.label &&
            <div className="checkbox-label">{this.props.label}</div>
          }
        </div>
      );
    }
  });
