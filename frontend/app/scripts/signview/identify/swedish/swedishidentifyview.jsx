define(["legacy_code",  "React", "common/backbone_mixin",
        "signview/identify/swedish/swedishidentifymodel", "signview/identify/swedish/swedishidentify",
        "signview/identify/swedish/swedishprocessing", "signview/identify/swedish/swedishproblem"],
  function (legacy_code, React, BackboneMixin,
            SwedishIdentifyModel, SwedishIdentify, SwedishProcessing, SwedishProblem) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(SwedishIdentifyModel).isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var model = this.props.model;
      return (
        <div>
          { /* if */ model.isIdentify() &&
            <SwedishIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <SwedishProcessing
              ref="processing"
              model={model}
            />
          }
          { /* else if */  model.isProblem() &&
            <SwedishProblem
              ref="problem"
              model={model}
            />
          }
        </div>
      );
    }
  });
});
