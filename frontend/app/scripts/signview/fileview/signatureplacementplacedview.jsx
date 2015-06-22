define(["React", "signview/fileview/placement_mixin", "legacy_code"], function (React, PlacementMixin) {
  return React.createClass({
    mixins: [PlacementMixin],

    activateSignatureModal: function () {
      var size = this.size();
      var field = this.props.model.field();

      new SignatureDrawOrTypeModal({
        field: field,
        width: size.width,
        height: size.height,
        arrow: this.props.arrow,
        signview: this.props.signview
      });
    },

    render: function () {
      var placement = this.props.model;
      var field = placement.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var image = field.value();
      var drawing = doc.signingInProcess() && doc.currentSignatoryCanSign() &&
        signatory.current() && !doc.readOnlyView();
      var hasImage = image !== "";

      var divClass = React.addons.classSet({
        "placedfield": true,
        "empty-signature": !hasImage
      });

      var divStyle = {
        cursor: drawing ? "pointer" : "",
        zIndex: drawing ? "1" : ""
      };

      _.extend(divStyle, this.position(0, 0, Math.round));

      var boxClass = React.addons.classSet({
        "signatureBox": drawing,
        "forDrawing": drawing,
        "signaturePlaceholder": !hasImage,
        "withImage": hasImage,
        "obligatory": field.obligatory(),
        "optional": !field.obligatory()
      });

      var size = this.size(_.identity);

      var boxStyle = {
        lineHeight: size.height + "px"
      };

      _.extend(boxStyle, size);

      return (
          <div className={divClass} style={divStyle}>
            {/* if */ (drawing || hasImage) &&
              <div className={boxClass} style={boxStyle} onClick={drawing && this.activateSignatureModal}>
                {/* if */ drawing && !hasImage &&
                  <span>{localization.signature.clickToDraw}</span>
                }
                {/* else if */ hasImage &&
                  <img src={image} style={size} />
                }
              </div>
            }
          </div>
      );
    }
  });
});
