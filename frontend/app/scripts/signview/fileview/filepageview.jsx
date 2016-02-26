var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SignaturePlacementPlacedView = require("./signatureplacementplacedview");
var CheckboxPlacementPlacedView = require("./checkboxplacementplacedview");
var TextPlacementPlacedView = require("./textplacementplacedview");
var FilePage = require("../../../js/files.js").FilePage;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FilePage).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      arrow: React.PropTypes.func.isRequired,
      imageSrc: React.PropTypes.string.isRequired,
      imageComplete: React.PropTypes.bool.isRequired,
      width: React.PropTypes.number.isRequired,
      height: React.PropTypes.number.isRequired
    },

    mixins: [BackboneMixin.BackboneMixin],

    // TODO: should be replaced with only `change` event later.
    componentWillMount: function () {
      this.props.model.on("change:dragables", this.handleChange);
    },

    componentWillUnmount: function () {
      this.props.model.off("change:dragables", this.handleChange);
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    renderFields: function () {
      var self = this;
      var page = self.props.model;
      var file = page.file();
      var width = self.props.width;
      var height = self.props.height;
      var doc = file.document();

      return _.map(doc.allPlacements(), function (placement, index) {
        if (placement.page() === page.number()) {
          var field = placement.field();

          var args = {
            model: placement,
            pageWidth: width,
            pageHeight: height,
            signview: self.props.signview,
            arrow: self.props.arrow
          };

          if (field.isSignature()) {
            return <SignaturePlacementPlacedView key={index} {...args} />;
          }

          if (field.isCheckbox()) {
            return <CheckboxPlacementPlacedView key={index} {...args} />;
          }

          if (field.isText()) {
            return <TextPlacementPlacedView key={index} {...args} />;
          }

          throw new Error("unknown field type");
        }
      });
    },

    handleChange: function () {
      if (this.isMounted()) {
        this.forceUpdate();
      }
    },

    render: function () {
      var page = this.props.model;
      var file = page.file();
      var doc = file.document();
      var imageSrc = this.props.imageSrc;
      var imageComplete = this.props.imageComplete;

      var pageStyle = {
        width: this.props.width + "px",
        height: this.props.height + "px"
      };

      return (
        <div style={pageStyle} id={"page" + page.number()} className="pagediv">
          <img src={imageSrc} />
          {/* if */ imageComplete && !doc.closed() &&
            this.renderFields()
          }
        </div>
      );
    }
  });
