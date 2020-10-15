var React = require("react");
var BackboneMixin = require("../../../common/backbone_mixin");
var NemIDIdentifyModel = require("./danishidentifymodel");
var NemIDIdentify = require("./danishidentify");
var NemIDChooseCVRAuthMethod = require("./danishchoosecvrauthmethod");
var NemIDProcessing = require("./danishprocessing");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(NemIDIdentifyModel).isRequired
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
            <NemIDIdentify
              ref="identify"
              model={model}
            />
          }
          { /* if */ model.isChooseCVRAuthMethod() &&
            <NemIDChooseCVRAuthMethod
              ref="choosecvrauthmethod"
              model={model}
            />
          }

          { /* else if */ model.isProcessing() &&
            <NemIDProcessing
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
