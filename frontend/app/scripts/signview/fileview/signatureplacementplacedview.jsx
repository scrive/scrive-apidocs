define(["React", "signview/fileview/placement_mixin", "signview/tasks/task_mixin",
        "signview/signaturemodal/signaturemodal", "legacy_code"],
  function (React, PlacementMixin, TaskMixin, SignatureModal) {

  return React.createClass({
    mixins: [PlacementMixin, TaskMixin],

    createTasks: function () {
      var self = this;
      var placement = self.props.model;
      var field = placement.field();

      if (!field.signatory().current()) {
        return;
      }

      return [new PageTask({
        type: "field",
        field: field,
        isComplete: function () {
          return placement.field().readyForSign();
        },
        el: $(self.getDOMNode()),
        label: localization.docsignview.signature,
        pointSelector: ".button",
        onArrowClick: function () {
          self.activateSignatureModal();
        },
        tipSide: placement.tip()
      })];
    },

    activateSignatureModal: function () {
      var size = this.size();
      var field = this.props.model.field();

      new SignatureModal({
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
