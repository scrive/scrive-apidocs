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
        margin: 5,
        onArrowClick: function () {
          self.toggleCheck();
        },
        tipSide: placement.tip()
      })];
    },

    toggleCheck: function () {
      var field = this.props.model.field();
      var doc = field.signatory().document();
      var current = field.signatory() == doc.currentSignatory() &&
        doc.currentSignatoryCanSign();

      if (current) {
        if (field.value() == "") {
          field.setValue("CHECKED");
        } else {
          field.setValue("");
        }
      }
    },

    render: function () {
      var field = this.props.model.field();
      var doc = field.signatory().document();
      var current = field.signatory() == doc.currentSignatory() &&
        doc.currentSignatoryCanSign();
      var checked = field.value() !== "";

      var divClass = React.addons.classSet({
        "placedfield": true,
        "js-checkbox": true,
        "to-fill-now": current,
        "obligatory": field.obligatory()
      });

      var boxClass = React.addons.classSet({
        "placedcheckbox": current,
        "placedcheckbox-noactive": !current,
        "checked": checked
      });

      var width = Math.round(this.width());
      var height = Math.round(this.height());
      var top = Math.round(this.top() - FieldPlacementGlobal.placementBorder);
      var left = Math.round(this.left() - FieldPlacementGlobal.placementBorder);
      var borderWidth = this.borderWidth();

      if (!current && checked) {
        width += borderWidth;
        height += borderWidth;
      }

      var divStyle = {
        cursor: current ? "pointer" : "",
        top: top,
        left: left,
        width: width,
        height: height,
        borderWidth: borderWidth
      };

      var boxStyle = {
        backgroundSize: width + "px " + height + "px",
        width: width,
        height: height
      };

      return (
        <div onClick={this.toggleCheck} className={divClass} style={divStyle}>
          <div className={boxClass} style={boxStyle} />
        </div>
      );
    }
  });
});
