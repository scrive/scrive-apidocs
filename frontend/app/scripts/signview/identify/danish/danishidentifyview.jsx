var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var DanishIdentifyModel = require("./danishidentifymodel");
var DanishIdentify = require("./danishidentify");
var DanishProcessing = require("./danishprocessing");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(DanishIdentifyModel).isRequired
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
            <DanishIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <DanishProcessing
              ref="processing"
              model={model}
            />
          }
        </div>
      );
    }
  });
