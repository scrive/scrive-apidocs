var React = require("react");
var PlacementMixin = require("./placement_mixin");
var TaskMixin = require("../tasks/task_mixin");
var SignatureModal = require("../signaturemodal/signaturemodal");
var zoomTools = require("../../common/zoomtools");
var PageTask = require("../../../js/tasks.js").PageTask;
var $ = require("jquery");

  module.exports = React.createClass({
    _openTry: 0,
    _openModal: false,

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
          arrow: this.props.arrow,
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
      var queryPart = doc.currentSignatory() ? "?signature_id=" + doc.currentSignatory().signatoryid()  : "";
      if (field.value()) {
        return field.value()
      } else if (field.signatureFile()) {
        return "/api/frontend/documents/" + doc.documentid() + "/files/" + field.signatureFile() +
               "/image.png" + queryPart;
      }
    },
    render: function () {
      var placement = this.props.model;
      var field = placement.field();
      var signatory = field.signatory();
      var doc = signatory.document();
      var drawing = doc.pending() && doc.currentSignatoryCanSign() && signatory.current();
      var hasImage = field.value() !== "" || field.signatureFile() != undefined;

      var divClass = React.addons.classSet({
        "placedfield": true,
        "empty-signature": !hasImage
      });

      var boxClass = React.addons.classSet({
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

      divStyle.fontSize = (this.scale() * 16) + "px";

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
          <div className={divClass} style={divStyle}>
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
