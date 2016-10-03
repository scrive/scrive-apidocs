import React from "react";

module.exports = React.createClass({
  _image: null,

  displayName: "AuthorAttachmentPage",

  propTypes: {
    number: React.PropTypes.number.isRequired,
    imageSrc: React.PropTypes.string.isRequired,
    width: React.PropTypes.number
  },

  componentWillMount: function () {
    this._image = new Image();
    this._image.src = this.props.imageSrc;
    this._image.onload = () => { this.forceUpdate(); };
  },

  render: function () {
    const {number, imageSrc, width} = this.props;

    return (
      <div style={{width: (width + "px")}} id={"page" + number} className="pagediv">
        <img onDragStart={(e) => { e.preventDefault(); }} src={imageSrc} />
      </div>
    );
  }
});
