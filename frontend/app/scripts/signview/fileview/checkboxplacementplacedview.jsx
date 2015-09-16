define(["React", "signview/fileview/placement_mixin", "signview/tasks/task_mixin", "legacy_code"],
  function (React, PlacementMixin, TaskMixin) {
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
        label: localization.docsignview.checkbox,
        onArrowClick: function () {
          self.toggleCheck();
        },
        tipSide: placement.tip()
      })];
    },

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
