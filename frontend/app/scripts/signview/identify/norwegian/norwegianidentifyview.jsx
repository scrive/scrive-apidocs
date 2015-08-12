define(["legacy_code",  "React","common/backbone_mixin",
        "signview/identify/norwegian/norwegianidentifymodel","signview/identify/norwegian/norwegianidentify",
        "signview/identify/norwegian/norwegianprocessing"],
  function (legacy_code, React, BackboneMixin,
            NorwegianIdentifyModel, NorwegianIdentify, NorwegianProcessing) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(NorwegianIdentifyModel).isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var model = this.props.model;
      var doc = model.doc();
      var sig = doc.currentSignatory();
       var personalNumber = sig.personalnumber();
      return (
        <div>
          { /* if */ model.isIdentify() &&
            <NorwegianIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <NorwegianProcessing
              ref="processing"
              model={model}
            />
          }
        </div>
      );
    }
  });
});
