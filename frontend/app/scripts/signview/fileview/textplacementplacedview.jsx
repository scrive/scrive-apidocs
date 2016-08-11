var React = require("react");
var InfoTextInput = require("../../common/infotextinput");
var PlacementMixin = require("./placement_mixin");
var TaskMixin = require("../navigation/task_mixin");
var Task = require("../navigation/task");
var $ = require("jquery");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var classNames = require("classnames");
var isElementInViewport = require("../../common/iselementinviewport");
var isTouchDevice = require("../../common/is_touch_device");
var Track = require("../../common/track");

var ORIGINAL_PAGE_SIZE = 950;

  module.exports = React.createClass({
    displayName: "TextPlacement",
    _lastWidth: 0,

    mixins: [PlacementMixin, TaskMixin],

    contextTypes: {
      zoomToPoint: React.PropTypes.func
    },

    getInitialState: function () {
      return {editing: false, active: false};
    },

    componentWillUpdate: function (prevProps, prevState) {
      this._lastWidth = $(this.getDOMNode()).width(); // Width is mangled just before zooming.
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (!prevState.editing && this.state.editing) {
        this.focusInput();
      }
    },

    shouldZoomTo: function () {
      return this.props.pageWidth < ORIGINAL_PAGE_SIZE || isTouchDevice();
    },

    zoomTo: function () {
      var $node = $(this.getDOMNode());
      var offset = $node.offset();
      var middleX = offset.left + (this._lastWidth / 2);
      var middleY = offset.top - $(window).scrollTop() + ($node.height() / 2);
      var zoomPoint = {x: middleX, y: middleY};
      var zoom = ORIGINAL_PAGE_SIZE / $(window).width();
      this.context.zoomToPoint(zoomPoint, zoom);
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
        onActivate: function (task) {
          // Wait for other textplacements to blur.
          setTimeout(function () {
            self.setState({active: false});
            // It the window does not have focus (for some old browsers we can't really tell), we should not start
            // inline editing.
            var nothingHasFocus = $(":focus").length === 0;
            var windowIsFocused = window.document.hasFocus == undefined || window.document.hasFocus();
            if (nothingHasFocus && !field.readyForSign() && windowIsFocused && !isTouchDevice() && task.active()) {
              self.startInlineEditing();
              Track.track("Begin editing field", {Label: field.name()});
            }
          }, 50);
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
      if (this.refs.input && isElementInViewport.part(this.refs.input.getDOMNode())) {
        this.refs.input.focus();
        if (this.shouldZoomTo()) {
          this.zoomTo();
        }
      }
    },

    startInlineEditing: function () {
      if (this.canSign() && !this.state.editing && isElementInViewport.part(this.getDOMNode())) {
        this.setState({editing: true});
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
      this.accept();
    },

    handleChange: function (value) {
      var field = this.props.model.field();
      field.setValue(value);
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
        lineHeight: (divStyle.fontSize + extraLineHeight) + "px",
        display: !editing ? "block" : "none"
      };

      var textStyle = {
        fontSize: divStyle.fontSize + "px",
        lineHeight: "1",
        height: (divStyle.fontSize + extraLineHeight) + "px",
        borderWidth: "0px",
        padding: spacingString,
        display: editing ? "block" : "none"
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
          onClick={this.handleClick}
          className={divClass}
          style={divStyle}
        >
          <div className="placedfield-placement-wrapper">
            <div
              className={boxClass}
              style={boxStyle}
            >
              {field.nicetext()}
            </div>
          </div>
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
        </div>
      );
    }
  });
