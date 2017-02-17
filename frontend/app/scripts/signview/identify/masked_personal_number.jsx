var React = require("react");

var MaskedPersonalNumber = React.createClass({
  displayName: "MaskedPersonalNumber",
  propTypes: {
    number: React.PropTypes.string,
    placeholder: React.PropTypes.string,
    isNorwegian: React.PropTypes.bool
  },
  maskNumber: function (digits) {
    var result = this.props.number;
    var replacementCount = 0;
    for (var i = this.props.number.length - 1; i >= 0; i--) {
      if (!isNaN(parseInt(this.props.number.substr(i, 1), 10))) {
        result = this.props.number.substr(0, i) + "*" + result.substr(i + 1);
        replacementCount++;
      }

      if (replacementCount == digits) {
        break;
      }
    }

    return result;
  },
  render: function () {
    var result = this.props.placeholder || "";

    if (this.props.number) {
      if (this.props.isNorwegian) {
        result = this.maskNumber(5);
      } else {
        result = this.maskNumber(4);
      }
    }

    return <b>{result}</b>;
  }
});

module.exports = MaskedPersonalNumber;
