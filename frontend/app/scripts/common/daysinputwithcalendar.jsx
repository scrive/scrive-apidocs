/** @jsx React.DOM */

/**
 * DaysInputWithCalendar. Small text input with calendar button next to it. Designed to select number of days.
 */

define(['React', 'common/infotextinput', 'legacy_code'], function(React,InfoTextInput) {
  return React.createClass({
    propTypes: {
      infotext        : React.PropTypes.string,
      label           : React.PropTypes.string,
      className       : React.PropTypes.string,
      inputClassName  : React.PropTypes.string,
      labelClassName  : React.PropTypes.string,
      days            : React.PropTypes.number,
      minDays         : React.PropTypes.number,
      maxDays         : React.PropTypes.number,
      canBeEmpty      : React.PropTypes.bool,
      onChange        : React.PropTypes.func
    },
    onInputChange : function(value) {
      var days = parseInt(value);
      if (days != undefined && !isNaN(days) && (days + "" == value)) {
        days = Math.min(this.props.maxDays, days);
        days = Math.max(this.props.minDays, days);
        this.props.onChange(days);
      } else if (value == "" && this.props.canBeEmpty) {
        this.props.onChange(null);
      }

      if (value != "") {
        this.refs.input.setValue("" + (this.props.days || ""));
      }
    },
    hideCalendar : function() {
      if (this.calendar != undefined) {
        this.calendar.close();
      }
    },
    componentDidMount : function() {
      var self = this;
      self.calendar = new Calendar({
        on : $(this.refs.calendarbutton.getDOMNode()),
        days : this.props.days,
        maxValue : this.props.maxDays,
        change: function(days) {
          self.props.onChange(days);
        }
      });
    },
    componentDidUpdate : function() {
      if (this.calendar != undefined) {
        this.calendar.setDays(this.props.days);
        this.calendar.setMax(this.props.maxDays);
      }
    },
    render: function() {
      var self = this;
      return (
        <div className={this.props.className}>
          <InfoTextInput
            ref="input"
            infotext={this.props.infotext}
            value={"" +  (this.props.days || "")}
            className={this.props.inputClassName}
            onChange={function(value) {self.onInputChange(value);}}
            onBlur={function() {
              // There can be trash empty in input. We should put back current value
              self.refs.input.setValue("" +  (this.props.days || ""));
            }}
          />
         <div className={this.props.labelClassName}>
           {this.props.label}
         </div>
         <div className="calendarbutton" ref="calendarbutton"/>
       </div>
      );
    }
  });
});
