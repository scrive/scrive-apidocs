var React = require("react");
var PlacementMixin = require("./placement_mixin");
var TaskMixin = require("../navigation/task_mixin");
var SignatureModal = require("../signaturemodal/signaturemodal");
var zoomTools = require("../../common/zoomtools");
var Task = require("../navigation/task");
var TaskList = require("../navigation/task_list");
var $ = require("jquery");
var classNames = require("classnames");
var _ = require("underscore");
var ModelObserverMixin = require("../model_observer_mixin");

var FONT_SIZE_BASE = 16;
var SIGNATURE_PLACEMENT_WIDTH_BASE = 284;

  module.exports = React.createClass({
    _openTry: 0,
    _openModal: false,

    displayName: "SignaturePlacementPlacedView",
    mixins: [PlacementMixin, TaskMixin, ModelObserverMixin],

    contextTypes: {
      taskList: React.PropTypes.instanceOf(TaskList),
      highlightingEnabled: React.PropTypes.func,
      hideArrow: React.PropTypes.func,
      showArrow: React.PropTypes.func
    },

    shouldComponentUpdate: function (nextProps, nextState) {
      var observedFieldsModel = ["wrel", "hrel", "xrel", "yrel", "fsrel"];
      var observedFieldsField = ["value", "signature", "is_obligatory"];
      var observedFieldsSignatory = ["current"];
      var observedFieldsDocument = ["status"];

      var result = (
        (nextProps.pageWidth != this.props.pageWidth)
        || (nextProps.pageHeight != this.props.pageHeight)
        || this.hasChanges(this.props.model, observedFieldsModel)
        || this.hasChanges(this.props.model.field(), observedFieldsField)
        || this.hasChanges(this.props.model.field().signatory(), observedFieldsSignatory)
        || this.hasChanges(this.props.model.field().signatory().document(), observedFieldsDocument)
      );

      return result;
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
          return !self._openModal && placement.field().readyForSign();
        },
        el: $(self.getDOMNode()),
        pointSelector: ".button",
        onArrowClick: function () {
          self.activateSignatureModal();
        },
        tipSide: placement.tip()
      })];
    },

    activateSignatureModal: function () {
      var self = this;
      var field = this.props.model.field();

      var width = window.innerWidth;
      var zoom = zoomTools.zoomLevel();

      if (zoom > 1 && width < 772) {
        if (self._openTry === 0) {
          alert(localization.zoomOut1 + "\n" + localization.zoomOut2);
        } else {
          alert(localization.zoomOutMore);
        }

        self._openTry += 1;

        return;
      }

      if (!self._openModal) {
        self._openModal = true;
        self._openTry = 0;
        new SignatureModal({
          field: field,
          width: this.width(),
          height: this.height(),
          hideArrow: this.context.hideArrow,
          showArrow: this.context.showArrow,
          highlightingEnabled: this.context.highlightingEnabled,
          signview: this.props.signview,
          onClose: function () {
            self._openModal = false;
            field.trigger("change");
          }
        });
      }
    },
    imageSrc: function () {
      var placement = this.props.model;
      var field = placement.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var queryPart = doc.currentSignatory() ? "?signatory_id=" + doc.currentSignatory().signatoryid()  : "";
      if (field.value()) {
        return field.value();
      } else if (field.signatureFile()) {
        return "/api/frontend/documents/" + doc.documentid() + "/files/" + field.signatureFile() +
               "/image.png" + queryPart;
      }
    },
    onMouseDown: function (event) {
      event.stopPropagation();
      event.preventDefault();
    },
    render: function () {
      var placement = this.props.model;
      var field = placement.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var drawing = doc.pending() && doc.currentSignatoryCanSign() && signatory.current();
      var hasImage = field.value() !== "" || field.signatureFile() != undefined;

      var divClass = classNames({
        "placedfield": true,
        "empty-signature": !hasImage
      });

      var boxClass = classNames({
        "signatureBox": drawing,
        "forDrawing": drawing,
        "signaturePlaceholder": !hasImage,
        "withImage": hasImage,
        "obligatory": field.obligatory(),
        "optional": !field.obligatory()
      });

      var width = this.width();
      var height = this.height();
      var top = Math.round(this.top());
      var left = Math.round(this.left());

      var divStyle = {
        cursor: drawing ? "pointer" : "",
        zIndex: drawing ? "1" : "",
        top: top,
        left: left
      };

      var ratio = width / (this.scale() * SIGNATURE_PLACEMENT_WIDTH_BASE);
      var fontSizeMin = this.scale() * FONT_SIZE_BASE;
      var fontSizeScaled = ratio * fontSizeMin;
      var fontSize = Math.max(fontSizeScaled, fontSizeMin);
      divStyle.fontSize = Math.round(fontSize) + "px";

      var boxStyle = {
        lineHeight: height + "px",
        cursor: drawing ? "pointer" : "",
        marginLeft: (-2 * this.scale()) + "px",
        marginTop: (-2 * this.scale()) + "px",
        borderWidth: this.borderWidth(),
        width: width,
        height: height
      };

      return (
          <div onMouseDown={this.onMouseDown} className={divClass} style={divStyle}>
            {/* if */ (drawing || hasImage) &&
              <div className={boxClass} style={boxStyle} onClick={drawing && this.activateSignatureModal}>
                {/* if */ drawing && !hasImage &&
                  <span>{localization.signature.clickToDraw}</span>
                }
                {/* else if */ hasImage &&
                  <img src={this.imageSrc()} style={{width: width, height: height}} />
                }
              </div>
            }
          </div>
      );
    }
  });
