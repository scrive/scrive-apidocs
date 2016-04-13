var React = require("react");
var $ = require("jquery");
var _ = require("underscore");
var classNames = require("classnames");

/*
Standard select boxes.

Example:
  <Select
    options={{name: "Option 1, value: 1}}
    onSelect={function (v) { console.log(v); }}
  />

Usage:

  <Select
    className="select-1" // extra class for select.
    style={{ backgroundColor: "white" }} // style for select box.
    width={100} // width of select button in pixels.
    isOptionSelected={function (o) { return o.value === 1}} // function for finding selected option
    onSelect={function (v) { return true; }} // fired when an option is selected,
                                             // can be overwittern by options onSelect param
    onRemove={function () { }} // if set adds a remove button to select box.
    options={[
      {
        name: "Option 1", // name to show in option box.
        value: 1, // value to pass to onSelect.
        onSelect: function () { }, // if set, fires when this option is selected.
        selected: true // is option selected. Checked by default isOptionSelected.
                       // If isOptionSelected is provided, this parameter may be ignored.
      },
      {
        name: "Option 2", // name to show in option box.
        value: 2, // value to pass to onSelect.
        onSelect: function () { }, // if set, fires when this option is selected.
        disabled: false // if true this option can't be selected.
      }
    ]}
  />

  NOTE: Select with less then two options will always be inactive.
*/

var EXTRA_BUTTON_WIDTH = 27;

module.exports = React.createClass({
  mixins: [React.addons.PureRenderMixin],

  propTypes: {
    name: React.PropTypes.string,
    isOptionSelected: React.PropTypes.func,
    className: React.PropTypes.string,
    width: React.PropTypes.number,
    style:  React.PropTypes.object,
    onSelect: React.PropTypes.func,
    onRemove: React.PropTypes.func,
    options: React.PropTypes.array.isRequired,
  },

  getDefaultProps: function () {
    return {
      style: {},
      width: 160,
      isOptionSelected: function (o) {return o.selected;}
    };
  },

  selectedOptionIndex: function () {
    var soi = _.findIndex(this.props.options, this.props.isOptionSelected);
    return soi > -1 ? soi : 0;
  },

  selectedOption: function () {
    return this.props.options[this.selectedOptionIndex()];
  },

  inactive: function () {
    return this.props.options.length < 2;
  },

  name: function () {
    var so =  this.selectedOption();
    return so ?  so.name : "";
  },

  select: function (index) {
    var option = this.props.options[index];
    if (option) {
      this.handleSelect(option);
    }
  },

  handleRemove: function (e) {
    e.stopPropagation();
    this.props.onRemove();
  },

  handleSelect: function (option) {
    var onSelect = option.onSelect || this.props.onSelect;
    if (onSelect) {
      onSelect(option.value);
    }
  },

  render: function () {
    var mainStyle = {maxWidth: this.props.width + "px"};
    var buttonStyle = {width: this.props.width + "px"};
    var labelStyle = {width: this.props.width - EXTRA_BUTTON_WIDTH + "px"};
    mainStyle = _.extend(mainStyle, this.props.style);
    labelStyle = _.extend(labelStyle, this.props.style);
    var mainClass = classNames(this.props.className, {
      "select": true,
      "inactive": this.inactive()
    });

    return (
      <div
        className={mainClass}
        style={mainStyle}
      >
        <select
          value={this.selectedOptionIndex()}
          onChange={ e => {
            if (e.target.value) {
              this.handleSelect(this.props.options[parseInt(e.target.value)]);
            }
          }}
        >
          {_.map(this.props.options, function (option, index) {
              return (
                <option
                  key={index}
                  value={index}
                  disabled={option.disabled === true}
                >
                  {option.name}
                </option>
              );
            })
          }
        </select>

        <div className="select-button" style={buttonStyle}>
          <div className="select-button-left"/>
          <div className="select-button-label" style={labelStyle}>
            {this.name()}
          </div>
          <div className="select-button-right"/>
          {/* if */ this.props.onRemove &&
            <div ref="close" className="closer" onClick={this.handleRemove}/>
          }
        </div>
      </div>
    );
  }
});
