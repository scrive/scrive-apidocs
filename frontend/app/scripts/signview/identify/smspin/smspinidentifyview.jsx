var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var SMSPinIdentifyModel = require("./smspinidentifymodel");
var SMSPinStartView = require("./smspinstartview");
var SMSPinFillView = require("./smspinfillview");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(SMSPinIdentifyModel).isRequired
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
          { /* if */ model.isOnStartStep() &&
            <SMSPinStartView
              ref="identify"
              model={model}
            />
          }
          { /* else if */ model.isOnFillStep() &&
            <SMSPinFillView
              ref="processing"
              model={model}
            />
          }
        </div>
      );
    }
  });
