var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var NorwegianIdentifyModel = require("./norwegianidentifymodel");
var NorwegianIdentify = require("./norwegianidentify");
var NorwegianProcessing = require("./norwegianprocessing");

  module.exports = React.createClass({
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
