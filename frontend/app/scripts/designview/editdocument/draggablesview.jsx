var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DraggableCheckbox = require("./draggablecheckbox");
var DraggableSignature = require("./draggablesignature");
var DraggableText = require("./draggabletext");
var Document = require("../../../js/documents.js").Document;

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      document: React.PropTypes.instanceOf(Document),
      proxiedShowCoordinateAxes: React.PropTypes.func.isRequired,
      proxiedHideCoordinateAxes: React.PropTypes.func.isRequired,
      proxiedMoveCoordinateAxes: React.PropTypes.func.isRequired,
      proxiedOpenTypeSetterFor: React.PropTypes.func.isRequired
    },

    getBackboneModels: function () {
      return [this.props.document];
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
          {this.props.document.ready() &&
            <span>
             <DraggableText
              document={this.props.document}
              showCoordinateAxes={this.props.proxiedShowCoordinateAxes()}
              hideCoordinateAxes={this.props.proxiedHideCoordinateAxes()}
              moveCoordinateAxes={this.props.proxiedMoveCoordinateAxes()}
              openTypeSetterFor={this.props.proxiedOpenTypeSetterFor()}
             />
             <DraggableSignature
              document={this.props.document}
              showCoordinateAxes={this.props.proxiedShowCoordinateAxes()}
              hideCoordinateAxes={this.props.proxiedHideCoordinateAxes()}
              moveCoordinateAxes={this.props.proxiedMoveCoordinateAxes()}
              openTypeSetterFor={this.props.proxiedOpenTypeSetterFor()}
             />
             <DraggableCheckbox
              document={this.props.document}
              showCoordinateAxes={this.props.proxiedShowCoordinateAxes()}
              hideCoordinateAxes={this.props.proxiedHideCoordinateAxes()}
              moveCoordinateAxes={this.props.proxiedMoveCoordinateAxes()}
              openTypeSetterFor={this.props.proxiedOpenTypeSetterFor()}
             />
            </span>
          }
        </div>
      );
    }
  });
