define(["React", "signview/fileview/placement_mixin", "legacy_code"], function (React, PlacementMixin) {
  return React.createClass({
    mixins: [PlacementMixin],

    toggleCheck: function () {
      var field = this.props.model.field();

      if (field.value() == "") {
        field.setValue("CHECKED");
      } else {
        field.setValue("");
      }
    },

    render: function () {
      var field = this.props.model.field();
      var doc = field.signatory().document();
      var current = field.signatory() == doc.currentSignatory() &&
        doc.currentSignatoryCanSign();

      var divClass = React.addons.classSet({
        "placedfield": true,
        "to-fill-now": current,
        "obligatory": field.obligatory()
      });

      var divStyle = {
        cursor: current ? "pointer" : ""
      };

      _.extend(divStyle, this.position(FieldPlacementGlobal.placementBorder,
        FieldPlacementGlobal.placementBorder, Math.round));
      _.extend(divStyle, this.size(Math.round));

      var boxClass = React.addons.classSet({
        "placedcheckbox": current,
        "placedcheckbox-noactive": !current,
        "checked": field.value() !== ""
      });

      return (
        <div className={divClass} style={divStyle}>
          <div onClick={this.toggleCheck} className={boxClass} />
        </div>
      );
    }
  });
});
