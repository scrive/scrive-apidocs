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
  render: function () {
    var result = this.props.placeholder || "";

    if (this.props.number) {
      if (this.props.isNorwegian) {
        result = this.maskNumber(5);
      } else if (this.props.isDanish) {
        result = this.maskNumber(4);
      } else if (this.props.isFinnish) {
        result = this.maskNumber(4);
      } else {
        result = this.maskNumber(4);
      }
    }

    return <b>{result}</b>;
  }
});

module.exports = MaskedPersonalNumber;
