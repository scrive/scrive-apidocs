
define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin",
        "signview/fileview/signatureplacementplacedview", "signview/fileview/checkboxplacementplacedview",
        "signview/fileview/textplacementplacedview"],
  function (legacy_code, _, Backbone, React, BackboneMixin, SignaturePlacementPlacedView,
            CheckboxPlacementPlacedView, TextPlacementPlacedView) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FilePage).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      arrow: React.PropTypes.func.isRequired,
      imageSrc: React.PropTypes.string.isRequired,
      imageComplete: React.PropTypes.bool.isRequired,
      imageWidth: React.PropTypes.number.isRequired,
      imageHeight: React.PropTypes.number.isRequired
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
      var imageWidth = self.props.imageWidth;
      var imageHeight = self.props.imageHeight;

      return _.map(page.placements(), function (placement, index) {
        var field = placement.field();

        var args = {
          model: placement,
          width: imageWidth,
          height: imageHeight,
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
      });
    },

    handleChange: function () {
      if (this.isMounted()) {
        this.forceUpdate();
      }
    },

    render: function () {
      var page = this.props.model;
      var imageSrc = this.props.imageSrc;
      var imageComplete = this.props.imageComplete;

      return (
        <div id={"page" + page.number()} className="pagediv">
          <img src={imageSrc} />
          {/* if */ imageComplete &&
            this.renderFields()
          }
        </div>
      );
    }
  });
});
