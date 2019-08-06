var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var VerimiIdentifyModel = require("./verimiidentifymodel");
var VerimiIdentify = require("./verimiidentify");
var VerimiProcessing = require("./verimiprocessing");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(VerimiIdentifyModel).isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var model = this.props.model;
      var doc = model.doc();
      var sig = doc.currentSignatory();
      return (
        <div>
          { /* if */ model.isIdentify() &&
            <VerimiIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <VerimiProcessing
              ref="processing"
              model={model}
            />
          }
          { /* else if */ model.isLoading() &&
            <div className="loadingSpinner"/>
          }
        </div>
      );
    }
  });
