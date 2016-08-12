var React = require("react");
var CreateFieldsMixin = require("./createfieldsmixin");
var DraggableMixin = require("./draggablemixin");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var $ = require("jquery");

  module.exports = React.createClass({
    mixins: [CreateFieldsMixin, DraggableMixin],

    newTextExpectedSize: function () {
      return {
        width: 20 + 8 * this.newTextField().nicetext().length,
        height: 34
      };
    },

    newTextField: function () {
      var document = this.document();
      var signatory = document.signatoriesWhoSign()[0] || document.author();
      return signatory.fstnameField();
    },

    newTextPlacement: function (args) {
       var field = this.newTextField();
       var newPlacement = new FieldPlacement({
         field: field,
         page: args.page,
         xrel: args.xrel,
         yrel: args.yrel,
         fsrel: args.fsrel
       });
       field.addPlacement(newPlacement);
       return newPlacement;
    },

    newTextPlacementFromPosition: function (args) {
      return this.newTextPlacement({
        page: args.page,
        xrel: args.x / args.pageWidth,
        yrel: args.y / args.pageHeight,
        fsrel: FieldPlacementGlobal.fontSizeNormal / args.pageWidth
      });
    },

    textHelper: function () {
      var field = this.newTextField();
      var helper = $("<div class='placedfieldvalue value'/>");
      helper.attr("style", "padding: 5px 7px; font-size: 16px; line-height: 20px;");
      helper.addClass(FieldPlacementGlobal.signatoryCSSClass(field.signatory()));
      helper.text(field.nicetext());
      return helper;
    },

    componentDidMount: function () {
      var self = this;
      var el = $(this.getDOMNode());
      self.initializeOnClickCreatePlacement({
        el: el,
        isEnabledFunc: function () { return true; },
        expectedSizeFunc: self.newTextExpectedSize,
        newPlacementFromPosition: self.newTextPlacementFromPosition,
        openTypeSetterFor: self.props.openTypeSetterFor
      });
      self.initializeDraggable({
        el: el,
        cursorAt: {top: 7, left: 7},
        verticalOffset: -1,
        xAxisOffset: FieldPlacementGlobal.textPlacementXOffset,
        yAxisOffset: FieldPlacementGlobal.textPlacementYOffset,
        dropXOffset: FieldPlacementGlobal.textPlacementXOffset,
        dropYOffset: FieldPlacementGlobal.textPlacementYOffset,
        helper: self.textHelper,
        onDropOnPage: function (page, x, y, pageW, pageH) {
          var newTextPlacement = self.newTextPlacement({
            page: page,
            xrel: x / pageW,
            yrel: y / pageH,
            fsrel: FieldPlacementGlobal.fontSizeNormal / pageW
          });
          self.props.openTypeSetterFor(newTextPlacement);
        }
      });
    },

    render: function () {
      return (
        <div className="design-view-action-document-draggable-textbox design-view-action-document-draggable">
          <div className="design-view-action-document-draggable-wrapper">
            <div className="design-view-action-document-draggable-inner-wrapper">
              <div className="design-view-action-document-draggable-icon-wrapper">
                <div className="design-view-action-document-draggable-icon" />
              </div>
              <div className="design-view-action-document-draggable-text">
                <span>{localization.designview.freeTextBox}</span>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
