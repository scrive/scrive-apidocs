define(["legacy_code", "Backbone", "React", "common/backbone_mixin",
        "designview/typesetters/signaturetypesetterview", "common/htmltextwithsubstitution",
        "designview/editdocument/draggablemixin", "designview/fileview/hastypesettermixin"],
  function (legacy_code, Backbone, React, BackboneMixin, SignatureTypeSetterView,
            HtmlTextWithSubstitution, DraggableMixin, HasTypeSetterMixin) {

 return React.createClass({
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
      return SignatureTypeSetterView;
    },

    componentDidMount: function () {
      this.initResizeable();
      this.initDraggable();
    },

    initResizeable: function () {
      var self = this;
      var placement = this.getPlacement();
      var signatureBox = $(this.refs.signatureBox.getDOMNode());

      signatureBox.resizable({
        stop: function (e, ui) {
          _.each(placement.field().placements(), function (p) {
            p.set({
              wrel: signatureBox.width() / self.props.pageWidth,
              hrel: signatureBox.height() / self.props.pageHeight
            });
          });
        },
        resize: function (e, ui) {

          if (ui.size.width < 44) {
            ui.size.width = 44;
            signatureBox.width(44);
          }
          if (ui.size.height < 22) {
            ui.size.height = 22;
            signatureBox.height(22);
          }

          if (ui.size.height >  ui.size.width) {
            signatureBox.height(ui.size.width);
          } else if (3 * ui.size.height <  ui.size.width) {
            signatureBox.height(ui.size.width / 3);
          }

        }

      });
    },
    initDraggable: function () {
      var self = this;
      var placement = this.getPlacement();
      var document = placement.field().signatory().document();
      var signatureBox = $(this.refs.signatureBox.getDOMNode());

      self.initializeDraggable({
        el: $(self.getDOMNode()),
        verticalOffset: 0,
        xAxisOffset: 0,
        yAxisOffset: 0,
        dropXOffset: FieldPlacementGlobal.signaturePlacementDDLeftOffset,
        dropYOffset: FieldPlacementGlobal.signaturePlacementDDTopOffset,
        helper: signatureBox,
        onStart: self.closeTypeSetter,
        onDropOnPage: function (page, x, y, pageW, pageH) {
          var oldPage = document.file().page(placement.page());
          var newPage = document.file().page(page);
          placement.set({
            page: page,
            xrel: x / pageW,
            yrel: y / pageH,
            wrel: signatureBox.width() / pageW,
            hrel: signatureBox.height() / pageH
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
          className="placedfield js-signature empty-signature"
          style={{
            left: Math.round(placement.xrel() * self.props.pageWidth),
            top: Math.round(placement.yrel() * self.props.pageHeight)
          }}
          onClick={self.toogleTypeSetterAndCloseOther}
        >
          <div
            ref="signatureBox"
            className={"signatureBox " + FieldPlacementGlobal.signatoryCSSClass(signatory)}
            style={{
              width: Math.round(placement.wrel() * self.props.pageWidth),
              height: Math.round(placement.hrel() * self.props.pageHeight)
            }}
           >
            <div className="signatureDDIcon"/>
            <div className="signatureHeader">
              <HtmlTextWithSubstitution
                secureText={localization.signature.placeFor}
                subs={{".put-name-here": sname}}
              />
            </div>
          </div>
        </div>
      );
    }
  });
});
