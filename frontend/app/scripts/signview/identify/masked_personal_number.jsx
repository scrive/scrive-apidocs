var React = require("react");

var MaskedPersonalNumber = React.createClass({
  displayName: "MaskedPersonalNumber",
  propTypes: {
    number: React.PropTypes.string,
    placeholder: React.PropTypes.string,
    isNorwegian: React.PropTypes.bool,
    isDanish: React.PropTypes.bool,
    isFinnish: React.PropTypes.bool
  },
  maskNumber: function (digits) {
    //   "*".repeat(digits) is not supported by some browsers - like PhantomJS :(
    return this.props.number.substr(0, this.props.number.length - digits) + Array(digits + 1).join("*");
  },
  maskNumberText: function () {
    var result = this.props.placeholder || "";

    if (this.props.number) {
      if (this.props.isNorwegian) {
        return this.maskNumber(5);
      } else if (this.props.isDanish) {
        return this.maskNumber(4);
      } else if (this.props.isFinnish) {
        return this.maskNumber(4);
      } else {
        return this.maskNumber(4);
      }
    }
    return "";
  },
  render: function () {
    return <b>{this.maskNumberText()}</b>;
  }
});

module.exports = MaskedPersonalNumber;
