var React = require("react");
var CreateFieldsMixin = require("./createfieldsmixin");
var DraggableMixin = require("./draggablemixin");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var Field = require("../../../js/fields.js").Field;
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var $ = require("jquery");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

  module.exports = React.createClass({
    mixins: [CreateFieldsMixin, DraggableMixin],

    newCheckboxExpectedSize: function () {
      return {
        width: FieldPlacementGlobal.checkboxSprite,
        height: FieldPlacementGlobal.checkboxSprite
      };
    },

    newCheckboxPlacement: function (args) {
      var document = this.document();
      var signatory = document.signatoriesWhoSign()[0] || document.author();
      var field = new Field({
        type: "checkbox",
        signatory: signatory,
        is_checked: signatory.author(),
        name: document.newCheckboxName()
      });
      signatory.addField(field);
      var newPlacement = new FieldPlacement({
        field: field,
        page: args.page,
        xrel: args.xrel,
        yrel: args.yrel,
        wrel: args.wrel,
        hrel: args.hrel
      });
      field.addPlacement(newPlacement);
      return newPlacement;
    },

    newCheckboxPlacementFromPosition: function (args) {
      return this.newCheckboxPlacement({
        page: args.page,
        xrel: args.x / args.pageWidth,
        yrel: args.y / args.pageHeight,
        wrel: FieldPlacementGlobal.checkboxSprite / args.pageWidth,
        hrel: FieldPlacementGlobal.checkboxSprite / args.pageHeight
      });
    },

    checkboxHelper: function () {
      var document = this.document();
      var signatory = document.signatoriesWhoSign()[0] || document.author();
      var helper = $("<div class='placedcheckbox'/>");
      helper.addClass(FieldPlacementGlobal.signatoryCSSClass(signatory));
      helper.height(FieldPlacementGlobal.checkboxHeight);
      helper.width(FieldPlacementGlobal.checkboxWidth);
      if (signatory.author()) {
        helper.addClass("checked");
      }
      return helper;
    },

    disableDragAndClickIfNoneCanSign: function (el) {
      var self = this;
      el.mousedown(function () {
        if (self.document().signatoriesWhoSign().length > 0) {
          el.draggable("enable");
        } else {
          new FlashMessage({type: "error", content: localization.designview.dndDisabled});
          el.draggable("disable");
          return false;
        }
      });
    },

    componentDidMount: function () {
      var self = this;
      var el = $(this.getDOMNode());

      self.disableDragAndClickIfNoneCanSign(el);

      self.initializeOnClickCreatePlacement({
        el: el,
        isEnabledFunc: function () { return self.document().signatoriesWhoSign().length > 0; },
        expectedSizeFunc: self.newCheckboxExpectedSize,
        newPlacementFromPosition: self.newCheckboxPlacementFromPosition,
        openTypeSetterFor: self.props.openTypeSetterFor
      });

      self.initializeDraggable({
        el: el,
        cursorAt: {top: 7, left: 7},
        verticalOffset: 0,
        xAxisOffset: 0,
        yAxisOffset: 0,
        dropXOffset: FieldPlacementGlobal.checkboxPlacementDDLeftOffset,
        dropYOffset: FieldPlacementGlobal.checkboxPlacementDDTopOffset,
        helper: self.checkboxHelper,
        onDropOnPage: function (page, x, y, pageW, pageH) {
          var newCheckboxPlacement = self.newCheckboxPlacement({
            page: page,
            xrel: x / pageW,
            yrel: y / pageH,
            wrel: FieldPlacementGlobal.checkboxSprite / pageW,
            hrel: FieldPlacementGlobal.checkboxSprite / pageH
          });
          self.props.openTypeSetterFor(newCheckboxPlacement);
        }
      });
    },

    render: function () {
      return (
        <div className="design-view-action-document-draggable design-view-action-document-draggable-checkbox">
          <div className="design-view-action-document-draggable-wrapper">
            <div className="design-view-action-document-draggable-inner-wrapper">
              <div className="design-view-action-document-draggable-icon-wrapper">
                <div className="design-view-action-document-draggable-icon" />
              </div>
              <div className="design-view-action-document-draggable-text">
                <span>{localization.designview.checkbox}</span>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
