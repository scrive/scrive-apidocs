var React = require("react");

module.exports = React.createClass({
  displayName: "Page",

  propTypes: {
    number: React.PropTypes.number.isRequired,
    imageSrc: React.PropTypes.string.isRequired,
    width: React.PropTypes.number
  },

  render: function () {
    const number = this.props.number;
    const imageSrc = this.props.imageSrc;

    const pageStyle = {
      width: this.props.width + "px"
    };

    return (
      <div style={pageStyle} id={"page" + number} className="pagediv">
        <img onDragStart={(e) => { e.preventDefault(); }} src={imageSrc} />
        {this.props.children}
      </div>
    );
  }
});
