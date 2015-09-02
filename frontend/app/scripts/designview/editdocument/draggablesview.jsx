/** @jsx React.DOM */

define(["legacy_code", "React", "common/backbone_mixin", "designview/editdocument/help", "designview/editdocument/draggable"],
  function (legacy_code, React, BackboneMixin, Help, Draggable) {
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

      if (doc.signatoriesWhoSign()[0]) {
        var signatory = doc.signatoriesWhoSign()[0];
        var field = signatory.field("fstname", "standard");
        return field;
      }

      console.warn("no signatories in document, draggable could not be created");
    },

    signatureFactory: function () {
      var doc = this.props.model.document();

      var field = new Field({
        fresh: false,
        ddSignature: true,
        type: "signature",
        signatory: doc.signatoriesWhoSign()[0],
        name: "temp-signature"
      });

      return field;
    },

    checkboxFactory: function () {
      var doc = this.props.model.document();

      var field = new Field({
        fresh: false,
        type: "checkbox",
        value: "checked",
        signatory: doc.signatoriesWhoSign()[0],
        name: "temp-checkbox"
      });

      return field;
    },

    render: function () {
      var doc = this.props.model.document();

      return (
        <div className="design-view-action-document-draggables">
          <Help className="help1" text={localization.designview.draggablehelp1} />
          <Help className="help2" text={localization.designview.draggablehelp2} />
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
