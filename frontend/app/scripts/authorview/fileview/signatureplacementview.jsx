var React = require("react");
var classNames = require("classnames");

var FieldPlacement = require("../../../js/placements.js").FieldPlacement;

module.exports = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
    pageWidth: React.PropTypes.number.isRequired,
    pageHeight: React.PropTypes.number.isRequired
  },
  fieldStyle: function () {
    var pageWidth = this.props.pageWidth;
    var pageHeight = this.props.pageHeight;

    var fieldStyle = {
      height: this.props.model.hrel() * pageHeight,
      left: Math.round(this.props.model.xrel() * pageWidth),
      top: Math.round(this.props.model.yrel() * pageHeight),
      width: this.props.model.wrel() * pageWidth
    };

    if (!this.props.model.field().signatureFile()) {
      fieldStyle.display = "none";
    }

    return fieldStyle;
  },
  imageSrc: function () {
    var field = this.props.model.field();
    var signatory = field.signatory();
    var doc = signatory.document();

    var qsParts = [];
    if (doc.currentSignatory()) {
      qsParts.push("signatory_id=" + doc.currentSignatory().signatoryid());
    }

    var srcParts = [
      "/api/frontend/documents/", doc.documentid(), "/files/",
      field.signatureFile(), "/image.png"
    ];

    if (qsParts.length > 0) {
      srcParts.push("?", qsParts.join("&"));
    }

    return srcParts.join("");
  },
  imageStyle: function () {
    var fieldStyle = this.fieldStyle();
    return {
      height: fieldStyle.height,
      width: fieldStyle.width
    };
  },
  render: function () {
    var fieldClass = classNames("placedfield", {
      "empty-signature": (this.props.model.field().signatureFile() == undefined)
    });

    return (
      <div className={fieldClass} style={this.fieldStyle()}>
        <div>
          { /* if */ (this.props.model.field().signatureFile()) &&
            <img
              height={this.imageStyle().height}
              src={this.imageSrc()}
              style={this.imageStyle()}
              width={this.imageStyle().width}
            />
          }
        </div>
      </div>
    );
  }
});
