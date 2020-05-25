var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var SEBankIDIdentifyModel = require("./swedishidentifymodel");
var SEBankIDIdentify = require("./swedishidentify");
var SEBankIDProcessing = require("./swedishprocessing");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(SEBankIDIdentifyModel).isRequired
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
            <SEBankIDIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <SEBankIDProcessing
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
