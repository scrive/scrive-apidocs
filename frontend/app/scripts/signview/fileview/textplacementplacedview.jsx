define(["React", "common/infotextinput", "signview/fileview/placement_mixin", "signview/tasks/task_mixin",
  "legacy_code"],
  function (React, InfoTextInput, PlacementMixin, TaskMixin) {
  return React.createClass({
    mixins: [PlacementMixin, TaskMixin],

    getInitialState: function () {
      return {editing: false};
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (this.props.arrow()) {
        this.props.arrow().updatePosition();
      }

      if (!prevState.editing && this.state.editing) {
        this.focusInput();
      }
    },

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
        label: localization.docsignview.textfield,
        onArrowClick: function () {
          self.startInlineEditing();
        },
        tipSide: placement.tip()
      })];
    },

    focusInput: function () {
      if (this.refs.input) {
        var $window = $(window);
        var $input = $(this.refs.input.getDOMNode());
        if ($window.scrollTop() + $window.height() > $input.offset().top && $window.scrollTop() < $input.offset().top) {
          this.refs.input.focus();
        }
      }
    },

    startInlineEditing: function () {
      if (this.state.editing) {
        this.focusInput();
      } else {
        this.setState({editing: true});
      }
    },

    stopInlineEditing: function () {
      if (this.state.editing) {
        this.setState({editing: false});
      }
    },

    accept: function () {
      var field = this.props.model.field();
      var val = this.refs.input.value();
      field.setValue(val);
      field.signatory().trigger("change");
      field.trigger("change:inlineedited");
      this.stopInlineEditing();
    },

    componentWillUnmount: function () {
      console.log("unmount");
    },

    render: function () {
      var self = this;
      var field = self.props.model.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var current = signatory == doc.currentSignatory() && doc.currentSignatoryCanSign();
      var canSign = signatory.canSign() && !field.isClosed() &&
        field.signatory().current() && self.inlineediting != true &&
        !doc.readOnlyView();
      var editing = self.state.editing;

      var divClass = React.addons.classSet({
        "placedfield": true,
        "to-fill-now": canSign,
        "obligatory": field.obligatory(),
        "empty-text-field": field.value() === "",
        "active": editing
      });

      var divStyle = {
        cursor: current ? "pointer" : ""
      };

      if (!canSign && field.value() === "") {
        divStyle.display = "none";
      }

      var position = self.position(FieldPlacementGlobal.textPlacementXOffset - 1.5,
        FieldPlacementGlobal.textPlacementYOffset - 1.5);

      _.extend(divStyle, position);

      var boxClass = React.addons.classSet({
        "placedfieldvalue": true,
        "value": true
      });

      var boxStyle = {
        padding: FieldPlacementGlobal.textPlacementSpacingString,
        fontSize: divStyle.fontSize + "px",
        lineHeight: divStyle.fontSize +
          FieldPlacementGlobal.textPlacementExtraLineHeight + "px"
      };

      var textStyle = {
        fontSize: divStyle.fontSize + "px",
        lineHeight: "1",
        height: (divStyle.fontSize + 4) + "px",
        borderWidth: "0px",
        padding: FieldPlacementGlobal.textPlacementSpacingString
      };

      var inputStyle = {
        fontSize: divStyle.fontSize + "px",
        lineHeight: (divStyle.fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px",
        height: (divStyle.fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px",
        background: "transparent"
      };

      var okStyle = {
        fontSize: divStyle.fontSize + "px",
        lineHeight: (divStyle.fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px",
        height: (divStyle.fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight / 2) + "px"
      };

      return (
        <div className={divClass} style={divStyle}>
          {/* if */ !editing &&
            <div className="placedfield-placement-wrapper">
              <div
                className={boxClass}
                style={boxStyle}
                onClick={/* if */ canSign && self.startInlineEditing}
              >
                {field.nicetext()}
              </div>
            </div>
          }
          {/* else */ editing &&
            <InfoTextInput
              ref="input"
              infotext={field.nicename()}
              value={field.value()}
              style={textStyle}
              inputStyle={inputStyle}
              okStyle={okStyle}
              className="text-inline-editing"
              onEnter={self.accept}
              autoGrowth={true}
              onAutoGrowth={function () {
                if (self.props.arrow()) {
                  self.props.arrow().updatePosition();
                }
              }}
              onTab={self.accept}
              onBlur={self.accept}
              onOk={self.accept}
            />
          }
        </div>
      );
    }
  });
});
