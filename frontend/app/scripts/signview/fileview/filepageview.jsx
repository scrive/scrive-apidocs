var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SignaturePlacementPlacedView = require("./signatureplacementplacedview");
var CheckboxPlacementPlacedView = require("./checkboxplacementplacedview");
var TextPlacementPlacedView = require("./textplacementplacedview");
var FilePage = require("../../../js/files.js").FilePage;
var Page = require("../pageviewer/page");

  module.exports = React.createClass({
    displayName: "FilePageView",

    propTypes: {
      model: React.PropTypes.instanceOf(FilePage).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      image: React.PropTypes.instanceOf(Image).isRequired,
      width: React.PropTypes.number
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

    height: function () {
      if (!this.props.image.complete) {
        return 0;
      }

      return (this.props.width / this.props.image.width) * this.props.image.height;
    },

    renderFields: function () {
      var self = this;
      var page = self.props.model;
      var file = page.file();
      var width = self.props.width;
      var height = this.height();
      var doc = file.document();

      return _.map(doc.allPlacements(), function (placement, index) {
        if (placement.page() === page.number()) {
          var field = placement.field();

          var args = {
            model: placement,
            pageWidth: width,
            pageHeight: height,
            signview: self.props.signview
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
      var imageSrc = this.props.image.src;
      var imageComplete = this.props.image.complete;

      return (
        <Page width={this.props.width} number={page.number()} imageSrc={imageSrc}>
          {/* if */ imageComplete && !doc.closed() &&
            this.renderFields()
          }
        </Page>
      );
    }
  });
