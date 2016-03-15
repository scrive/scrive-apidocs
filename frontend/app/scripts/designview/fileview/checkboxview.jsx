var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CheckboxTypeSetterView = require("../typesetters/checkboxtypesetterview");
var DraggableMixin = require("../editdocument/draggablemixin");
var HasTypeSetterMixin = require("./hastypesettermixin");
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
      return CheckboxTypeSetterView;
    },

    componentDidMount: function () {
      this.initDraggable();
    },

    initDraggable: function () {
      var self = this;
      var placement = this.getPlacement();
      var document = placement.field().signatory().document();

      self.initializeDraggable({
        el: $(self.getDOMNode()),
        verticalOffset: 0,
        xAxisOffset: 0,
        yAxisOffset: 0,
        dropXOffset: FieldPlacementGlobal.checkboxPlacementDDLeftOffset,
        dropYOffset: FieldPlacementGlobal.checkboxPlacementDDTopOffset,
        onStart: self.closeTypeSetter,
        onDropOnPage: function (page, x, y, pageW, pageH) {
          var oldPage = document.file().page(placement.page());
          var newPage = document.file().page(page);
          placement.set({
            page: page,
            xrel: x / pageW,
            yrel: y / pageH,
            wrel: FieldPlacementGlobal.checkboxSprite / pageW,
            hrel: FieldPlacementGlobal.checkboxSprite / pageH
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

    render: function () {
      var self = this;
      var placement = this.getPlacement();
      var field = placement.field();
      var signatory = field.signatory();
      var sname = signatory.nameOrEmail();
      if (sname == "") {
        if (signatory.isCsv()) {
          sname = localization.csv.title;
        } else {
          sname = localization.process.signatoryname + " " + signatory.signIndex();
        }
      }
      return (
        <div
          className="placedfield js-checkbox"
          style={{
            left: Math.round(placement.xrel() * self.props.pageWidth) - FieldPlacementGlobal.placementBorder,
            top: Math.round(placement.yrel() * self.props.pageHeight) -  FieldPlacementGlobal.placementBorder
          }}
          onClick={self.toogleTypeSetterAndCloseOther}
        >
          <div
            className={
              "placedcheckbox " +
              FieldPlacementGlobal.signatoryCSSClass(signatory) + " "  +
              (field.isChecked() ? "checked" : "") + " " +
              (field.needsSenderAction() ? "needs-sender-action" : "")
            }
            style={{
              width: FieldPlacementGlobal.checkboxWidth,
              height: FieldPlacementGlobal.checkboxHeight
            }}
          />
        </div>
      );
    }
  });
