var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var SwedishIdentifyModel = require("./swedishidentifymodel");
var SwedishIdentify = require("./swedishidentify");
var SwedishProcessing = require("./swedishprocessing");
var SwedishProblem = require("./swedishproblem");

  module.exports = React.createClass({
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
