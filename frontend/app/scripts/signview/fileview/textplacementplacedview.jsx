var React = require("react");
var InfoTextInput = require("../../common/infotextinput");
var PlacementMixin = require("./placement_mixin");
var TaskMixin = require("../navigation/task_mixin");
var Task = require("../navigation/task");
var $ = require("jquery");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var classNames = require("classnames");

  module.exports = React.createClass({
    _shouldBlur: true,

    mixins: [PlacementMixin, TaskMixin],

    getInitialState: function () {
      var field = this.props.model.field();
      return {editing: false, active: false};
    },

    createTasks: function () {
      var self = this;
      var placement = self.props.model;
      var field = placement.field();

      if (!field.signatory().current()) {
        return;
      }

      return [new Task({
        type: "field",
        field: field,
        isComplete: function () {
          return field.readyForSign();
        },
        el: $(self.getDOMNode()),
        onArrowClick: function () {
          self.startInlineEditing();
        },
        onActivate: function () {
          setTimeout(function () {
            self.setState({active: true});
          }, 1);
          // It the window does not have focus (for some old browsers we can't really tell), we should not start
          // inline editing.
          var nothingHasFocus = $(":focus").size() == 0;
          var windowIsFocused = window.document.hasFocus == undefined || window.document.hasFocus();
          if (nothingHasFocus && !field.readyForSign() && windowIsFocused) {
            setTimeout(function () {
              self.startInlineEditing();
            }, 1);
            mixpanel.track("Begin editing field", {Label: field.name()});
          }
        },
        onDeactivate: function () {
          setTimeout(function () {
            self.setState({active: false});
          }, 1);
        },
        tipSide: placement.tip()
      })];
    },

    focusInput: function () {
      var self = this;
      if (self.refs.input) {
        var $window = $(window);
        var $input = $(self.refs.input.getDOMNode());
        if ($window.scrollTop() + $window.height() > $input.offset().top && $window.scrollTop() < $input.offset().top) {
          self.refs.input.focus();
        }
      }
    },

    startInlineEditing: function () {
      var self = this;
      if (this.canSign()) {
        if (this.state.editing) {
          this.focusInput();
        } else {
          this.setState({editing: true}, function () {
            self.focusInput();
          });
        }
      }
    },

    stopInlineEditing: function () {
      var self = this;
      var field = self.props.model.field();
      var tasks = self.props.signview.tasks();
      if (self.state.editing) {
        self.setState({editing: false}, function () {
          if (field.readyForSign()) {
            tasks.triggerOnActivate();
          }
        });
      }
    },

    accept: function () {
      this.stopInlineEditing();
    },

    handleBlur: function (e) {
      if (this._shouldBlur) {
        this.accept();
      }
    },

    handleChange: function (value) {
      var field = this.props.model.field();
      field.setValue(value);
    },

    handleMouseDown: function (e) {
      this._shouldBlur = false;
    },

    handleMouseUp: function (e) {
      this._shouldBlur = true;
    },

    handleClick: function (e) {
      this.startInlineEditing();
    },

    canSign: function () {
      var self = this;
      var field = self.props.model.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var current = signatory == doc.currentSignatory() && doc.currentSignatoryCanSign();
      return signatory.canSign() && !field.isClosed() &&
        field.signatory().current() && self.inlineediting != true;
    },

    render: function () {
      var self = this;
      var field = self.props.model.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var current = signatory == doc.currentSignatory() && doc.currentSignatoryCanSign();
      var editing = self.state.editing;

      var divClass = classNames({
        "placedfield": true,
        "placement-text": true,
        "to-fill-now": self.canSign(),
        "obligatory": field.obligatory(),
        "optional": !field.obligatory(),
        "empty-text-field": field.value() === "",
        "invalid": !field.readyForSign(),
        "active": editing
      });

      var top = this.top() - this.scale() * (FieldPlacementGlobal.textPlacementYOffset - 1.5);
      var left = this.left() - this.scale() * (FieldPlacementGlobal.textPlacementXOffset - 1.5);

      var divStyle = {
        cursor: current ? "text" : "",
        lineHeight: "normal",
        top: top,
        left: left,
        fontSize: this.fontSize(),
        borderWidth: this.borderWidth()
      };

      if (!self.canSign() && field.value() === "") {
        divStyle.display = "none";
      }

      var boxClass = classNames({
        "placedfieldvalue": true,
        "value": true
      });

      var extraLineHeight = this.scale() * FieldPlacementGlobal.textPlacementExtraLineHeight;
      var spacingString = (this.scale() * FieldPlacementGlobal.textPlacementVerSpace) + "px " +
        (this.scale() * FieldPlacementGlobal.textPlacementHorSpace) + "px";

      var boxStyle = {
        padding: spacingString,
        fontSize: divStyle.fontSize + "px",
        lineHeight: (divStyle.fontSize + extraLineHeight) + "px"
      };

      var textStyle = {
        fontSize: divStyle.fontSize + "px",
        lineHeight: "1",
        height: (divStyle.fontSize + extraLineHeight) + "px",
        borderWidth: "0px",
        padding: spacingString
      };

      var paddingRight = FieldPlacementGlobal.textPlacementHorSpace;
      var extraPadding = 8;

      if (this.state.active) {
        paddingRight += extraPadding;
      }

      textStyle.paddingRight = this.scale() * paddingRight;
      boxStyle.paddingRight = this.scale() * paddingRight;

      var inputStyle = {
        fontSize: divStyle.fontSize + "px",
        lineHeight: (divStyle.fontSize + extraLineHeight) + "px",
        height: (divStyle.fontSize + extraLineHeight) + "px",
        background: "transparent"
      };

      return (
        <div
          onMouseDown={this.handleMouseDown}
          onMouseUp={this.handleMouseUp}
          onTouchStart={this.handleClick}
          onClick={this.handleClick}
          className={divClass}
          style={divStyle}
        >
          {/* if */ !editing &&
            <div className="placedfield-placement-wrapper">
              <div
                className={boxClass}
                style={boxStyle}
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
              onChange={this.handleChange}
              style={textStyle}
              inputStyle={inputStyle}
              className="text-inline-editing"
              autoGrowth={true}
              onEnter={self.accept}
              onTab={(e) => { e.preventDefault(); self.accept(); }}
              onBlur={self.handleBlur}
            />
          }
        </div>
      );
    }
  });
