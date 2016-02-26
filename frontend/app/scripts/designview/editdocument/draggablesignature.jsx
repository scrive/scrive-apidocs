var React = require("react");
var CreateFieldsMixin = require("./createfieldsmixin");
var DraggableMixin = require("./draggablemixin");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var Field = require("../../../js/fields.js").Field;
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var $ = require("jquery");

  module.exports = React.createClass({
    mixins: [CreateFieldsMixin, DraggableMixin],

    newSignatureExpectedSize: function () {
      return {
        width: FieldPlacementGlobal.signatureWidth,
        height: FieldPlacementGlobal.signatureHeight
      };
    },

    newSignaturePlacement: function (args) {
      var document = this.document();
      var signatory = document.signatoriesWhoSign()[0] || document.author();
      var field = new Field({
        type: "signature",
        signatory: signatory,
        name: document.newSignatureName()
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

    newSignaturePlacementFromPosition: function (args) {
      return this.newSignaturePlacement({
        page: args.page,
        xrel: args.x / args.pageWidth,
        yrel: args.y / args.pageHeight,
        wrel: FieldPlacementGlobal.signatureWidth / args.pageWidth,
        hrel: FieldPlacementGlobal.signatureHeight / args.pageHeight
      });
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

    signatureHelper: function () {
      var document = this.document();
      var signatory = document.signatoriesWhoSign()[0] || document.author();
      var sname = signatory.nameOrEmail();
      if (sname == "") {
        if (signatory.isCsv()) {
          sname = localization.csv.title;
        } else {
          sname = localization.process.signatoryname + " " + signatory.signIndex();
        }
      }
      var helper = $("<div class='signatureBox'/>");
      helper.addClass(FieldPlacementGlobal.signatoryCSSClass(signatory));
      helper.height(FieldPlacementGlobal.signatureHeight);
      helper.width(FieldPlacementGlobal.signatureWidth);
      helper.append("<div class='signatureDDIcon'/>");
      var header = $("<div class='signatureHeader'/>");
      header.html(localization.signature.placeFor);
      header.find(".put-name-here").text(sname);
      helper.append(header);
      return helper;
    },

    componentDidMount: function () {
      var self = this;
      var el = $(this.getDOMNode());
      self.disableDragAndClickIfNoneCanSign(el);
      self.initializeOnClickCreatePlacement({
        el: el,
        isEnabledFunc: function () { return self.document().signatoriesWhoSign().length > 0;},
        expectedSizeFunc: self.newSignatureExpectedSize,
        newPlacementFromPosition: self.newSignaturePlacementFromPosition,
        openTypeSetterFor: self.props.openTypeSetterFor
      });
      self.initializeDraggable({
        el: el,
        cursorAt: {top:7, left:7},
        verticalOffset: 0,
        xAxisOffset: 0,
        yAxisOffset: 0,
        dropXOffset: FieldPlacementGlobal.signaturePlacementDDLeftOffset + FieldPlacementGlobal.signatureBorderWidth,
        dropYOffset: FieldPlacementGlobal.signaturePlacementDDTopOffset + FieldPlacementGlobal.signatureBorderWidth,
        helper: self.signatureHelper,
        onDropOnPage: function (page, x, y, pageW, pageH) {
          var newSignaturePlacement = self.newSignaturePlacement({
            page: page,
            xrel: x / pageW,
            yrel: y / pageH,
            wrel: FieldPlacementGlobal.signatureWidth / pageW,
            hrel: FieldPlacementGlobal.signatureHeight / pageH
          });
          self.props.openTypeSetterFor(newSignaturePlacement);
        }
      });
    },

    render: function () {
      return (
        <div className="design-view-action-document-draggable design-view-action-document-draggable-signature">
          <div className="design-view-action-document-draggable-wrapper">
            <div className="design-view-action-document-draggable-inner-wrapper">
              <div className="design-view-action-document-draggable-icon-wrapper">
                <div className="design-view-action-document-draggable-icon" />
              </div>
              <div className="design-view-action-document-draggable-text">
                <span>{localization.designview.signatureBox}</span>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
