var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DraggableCheckbox = require("./draggablecheckbox");
var DraggableSignature = require("./draggablesignature");
var DraggableText = require("./draggabletext");
var Document = require("../../../js/documents.js").Document;

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Document),
      showCoordinateAxes: React.PropTypes.func.isRequired,
      hideCoordinateAxes: React.PropTypes.func.isRequired,
      moveCoordinateAxes: React.PropTypes.func.isRequired,
      openTypeSetterFor: React.PropTypes.func.isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      return (
        <div className="design-view-action-document-draggables">
          <div className="design-view-action-document-draggables-help help1">
            <div className="wrapper">
              <div className="icon" />
              <div className="text-wrapper">
                <span className="text">
                  {localization.designview.draggablehelp1}
                </span>
              </div>
            </div>
          </div>
          <div className="design-view-action-document-draggables-help help2">
            <div className="wrapper">
              <div className="icon" />
              <div className="text-wrapper">
                <span className="text">
                  {localization.designview.draggablehelp2}
                </span>
              </div>
            </div>
          </div>
          {this.props.model.ready() &&
            <span>
             <DraggableText {...this.props}/>
             <DraggableSignature {...this.props}/>
             <DraggableCheckbox {...this.props}/>
            </span>
          }
        </div>
      );
    }
  });
