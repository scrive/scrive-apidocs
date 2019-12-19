var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var FinnishIdentifyModel = require("./finnishidentifymodel");
var FinnishInquiry = require("./finnishinquiry");
var FinnishIdentify = require("./finnishidentify");
var FinnishProcessing = require("./finnishprocessing");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(FinnishIdentifyModel).isRequired
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
          { /* if */ model.inquiryRequired() && model.isInquiry() &&
            <FinnishInquiry
              ref="inquiry"
              model={model}
            />
          }
          { /* else if */ model.isIdentify() &&
            <FinnishIdentify
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isProcessing() &&
            <FinnishProcessing
              ref="processing"
              model={model}
            />
          }
        </div>
      );
    }
  });
