var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var TextTypeSetterView = require("../typesetters/texttypesetterview");
var DraggableMixin = require("../editdocument/draggablemixin");
var HasTypeSetterMixin = require("./hastypesettermixin");
var InfoTextInput = require("../../common/infotextinput");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var $ = require("jquery");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
      pageWidth: React.PropTypes.number.isRequired,
      pageHeight: React.PropTypes.number.isRequired
    },

    mixins: [BackboneMixin.BackboneMixin, DraggableMixin, HasTypeSetterMixin],

    getBackboneModels: function () {
      return [this.getPlacement()];
    },

    getPlacement: function () {
      return this.props.model;
    },

    getTypeSetterClass: function () {
      return TextTypeSetterView;
    },

    getInitialState: function () {
      return {editing: false};
    },

    // Ugly hack for P2E middlewares. wrel and hrel is not used by our frontend, but is used by this middlewares.
    // Computation of this value is also hard - since it's almost impossible to compute it in backend.

    fixWHRel: function () {
      if (this.isMounted()) {
        var el = $(this.getDOMNode());
        this.getPlacement().set({
          wrel: el.width() / this.props.pageWidth,
          hrel: el.height() / this.props.pageHeight
        }, {silent: true});
      }
    },

    componentDidMount: function () {
      this.initDraggable();
    },

    componentDidUpdate: function () {
      setTimeout(this.fixWHRel, 100);
    },

    initDraggable: function () {
      var self = this;
      var placement = this.getPlacement();
      var document = placement.field().signatory().document();

      self.initializeDraggable({
        el: $(self.getDOMNode()),
        verticalOffset: -1,
        xAxisOffset: FieldPlacementGlobal.textPlacementXOffset,
        yAxisOffset: FieldPlacementGlobal.textPlacementYOffset,
        dropXOffset: FieldPlacementGlobal.textPlacementXOffset,
        dropYOffset: FieldPlacementGlobal.textPlacementYOffset,
        onStart: self.closeTypeSetter,
        onDropOnPage: function (page, x, y, pageW, pageH) {
          var oldPage = document.file().page(placement.page());
          var newPage = document.file().page(page);
          placement.set({
            page: page,
            xrel: x / pageW,
            yrel: y / pageH
          });
          oldPage.removePlacement(placement);
          newPage.addPlacement(placement);
        },
        onDropOutside: function () {
          placement.remove();
          placement.removeField();
        }
      });
    },

    openTypeSetterAndCloseOther: function () {
      if (!this.hasTypeSetter()) {
        this.props.closeAllTypeSetters();
        this.openTypeSetter();
      } else if (!this.editorIsAvailable()) {
        this.closeTypeSetter();
      }
    },

    editorIsAvailable: function () {
      var field = this.getPlacement().field();
      return !field.isCsvField() && !field.isAuthorUnchangeableField();
    },

    render: function () {
      var self = this;
      var placement = this.getPlacement();
      var field = placement.field();
      var signatory = field.signatory();
      var fontSize = Math.floor(placement.fsrel() * this.props.pageWidth);
      var hasEditor =  self.hasTypeSetter() && self.editorIsAvailable();
      return (
        <div
          className={"placedfield " + ("js-" + field.type()) + (field.isValid() ? "" : " invalid") }
          style={{
            left: Math.floor(placement.xrel() * this.props.pageWidth + 1.5) - FieldPlacementGlobal.textPlacementXOffset,
            top: Math.floor(placement.yrel() * this.props.pageHeight + 1.5) - FieldPlacementGlobal.textPlacementYOffset,
            fontSize: fontSize + "px"
          }}
          onClick={self.openTypeSetterAndCloseOther}
        >
          <div className="placedfield-placement-wrapper">
            { /* if */ hasEditor &&
              <InfoTextInput
                className={
                  "text-field-placement-setter-field-editor " +
                  FieldPlacementGlobal.signatoryCSSClass(signatory)
                }
                infotext={field.nicename()}
                value={field.value()}
                autoGrowth={true}
                focus={true}
                style={{
                  fontSize: fontSize + "px",
                   lineHeight: fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px",
                   height: fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px",
                   verticalAlign: "top",
                   padding: FieldPlacementGlobal.textPlacementSpacingString
                }}
                inputStyle={{
                  fontSize: fontSize + "px",
                  lineHeight: fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px",
                  height: fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px",
                  verticalAlign: "top"
                }}
                suppressSpace={field.isFstName()}
                onChange={function (val) {
                  field.setValue(val.trim());
                }}
                onEnter={self.closeTypeSetter}
                onAutoGrowth={self.forceUpdateTypeSetterIfMounted}
              />
            }

            { /* else */ !hasEditor &&
              <div
                className={"placedfieldvalue value " + FieldPlacementGlobal.signatoryCSSClass(signatory)}
                style={{
                  padding: FieldPlacementGlobal.textPlacementSpacingString,
                  fontSize: fontSize + "px",
                  lineHeight: fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px"
                }}
              >
                {field.nicetext()}
              </div>
            }
          </div>
        </div>
      );
    }
  });
