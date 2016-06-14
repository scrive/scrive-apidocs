var React = require("react");
var Backbone = require("backbone");
var Button = require("../../common/button");
var BackboneMixin = require("../../common/backbone_mixin");
var _ = require("underscore");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var self = this;
      var model = self.props.model;
      return (
        <div className="section signatory-attachment">
          <div className="col-sm-6 left">
            <h1>
              <span className="paperclip" />
              {model.name()}
            </h1>
            <p className="desc">{model.description()}</p>
            <p className="desc">
              <HtmlTextWithSubstitution
                secureText={localization.signatoryAttachmentAttachedBy}
                subs={{".put-name-here": model.signatory().name()}}
              />
            </p>
          </div>
          <div className="col-sm-6 right" ref="upload-or-load-area">
            <span>
              <div>
                <Button
                  text={localization.reviewPDF}
                  className="show-attachment"
                  onClick={function () {
                    window.open(model.file().downloadLink(), "_blank");
                  }}
                />
              </div>
            </span>
          </div>
        </div>
      );
    }
  });
