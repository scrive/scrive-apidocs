/** @jsx React.DOM */

define(["legacy_code", "React", "common/backbone_mixin", "designview/editdocument/draggablehelp", "designview/editdocument/draggable"],
  function (legacy_code, React, BackboneMixin, DraggableHelp, Draggable) {
  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    getBackboneModels: function () {
      return [this.props.model, this.props.model.document()];
    },

    textboxFactory: function () {
      var doc = this.props.model.document();
      var sig = doc.signatoriesWhoSign()[0] || doc.author();
      var field = sig.field("fstname", "standard");
      return field;
    },

    signatureFactory: function () {
      var doc = this.props.model.document();

      if (doc.signatoriesWhoSign()[0]) {
        return new Field({
          fresh: false,
          ddSignature: true,
          type: "signature",
          signatory: doc.signatoriesWhoSign()[0],
          name: "temp-signature"
        });
      }

      console.warn("no signatories in document, draggable could not be created");
    },

    checkboxFactory: function () {
      var doc = this.props.model.document();
      var sig = doc.signatoriesWhoSign()[0] || doc.author();

      return new Field({
        fresh: false,
        type: "checkbox",
        value: "checked",
        signatory: sig,
        name: "temp-checkbox"
      });

      console.warn("no signatories in document, draggable could not be created");
    },

    render: function () {
      var doc = this.props.model.document();

      return (
        <div className="design-view-action-document-draggables">
          <DraggableHelp className="help1" text={localization.designview.draggablehelp1} />
          <DraggableHelp className="help2" text={localization.designview.draggablehelp2} />
          {doc.ready() &&
            <span>
              <Draggable
                className="design-view-action-document-draggable-textbox"
                buttonText={localization.designview.freeTextBox}
                fieldFactory={this.textboxFactory}
                fontSize={16}
              />
              <Draggable
                className="design-view-action-document-draggable-signature"
                buttonText={localization.designview.signatureBox}
                fieldFactory={this.signatureFactory}
                onAdd={function (f) {
                  f.setName(doc.newSignatureName());
                }}
              />
              <Draggable
                className="design-view-action-document-draggable-checkbox"
                buttonText={localization.designview.checkbox}
                fieldFactory={this.checkboxFactory}
                onAdd={function (f) {
                  f.setName(doc.newCheckboxName());
                }}
              />
            </span>
          }
        </div>
      );
    }
  });
});
