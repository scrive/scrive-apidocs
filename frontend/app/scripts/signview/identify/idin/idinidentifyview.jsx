var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var IDINIdentifyModel = require("./idinidentifymodel");
var IDINIdentify = require("./idinidentify");
var IDINProcessing = require("./idinprocessing");

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  propTypes: {
    model: React.PropTypes.instanceOf(IDINIdentifyModel).isRequired
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
          <IDINIdentify
            ref="identify"
            model={model}
          />
        }
        { /* else if */ model.isProcessing() &&
          <IDINProcessing
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
