var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var NOBankIDIdentifyModel = require("./norwegianidentifymodel");
var NOBankIDIdentify = require("./norwegianidentify");
var NOBankIDProcessing = require("./norwegianprocessing");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(NOBankIDIdentifyModel).isRequired
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
            <NOBankIDIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <NOBankIDProcessing
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
