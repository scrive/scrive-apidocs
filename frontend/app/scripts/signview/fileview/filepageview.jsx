import  _ from "underscore";
import  Backbone from "backbone";
import  React from "react";
import  {FilePage} from "../../../js/files.js";
import  {Document} from "../../../js/documents.js";
import  {HighlightedPage} from "../../../js/highlightedpage.js";
import  BackboneMixin from "../../common/backbone_mixin";
import  SignaturePlacementPlacedView from "./signatureplacementplacedview";
import  CheckboxPlacementPlacedView from "./checkboxplacementplacedview";
import  TextPlacementPlacedView from "./textplacementplacedview";
import  Highlight from "./highlight";
import  RadioButtonPlacementPlacedView from "./radiobuttonplacementplacedview";

const DEFAULT_PIXEL_WIDTH = 1040;

module.exports = React.createClass({

  displayName: "FilePageView",

  propTypes: {
    filepage: React.PropTypes.instanceOf(FilePage).isRequired,
    document: React.PropTypes.instanceOf(Document).isRequired,
    signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
    image: React.PropTypes.object.isRequired,
    highlightingMode: React.PropTypes.bool,
    removingHighlightingMode: React.PropTypes.bool,
    onNewHighlight: React.PropTypes.func,
    onRemoveHighlighting: React.PropTypes.func,
    width: React.PropTypes.number
  },

  canEditHighlighting: function () {
    var doc = this.props.document;
    return doc.currentSignatoryCanSign() && doc.currentSignatory().allowshighlighting();
  },

  showHighlighting: function () {
    return !this.props.document.closed();
  },

  noneditableHighlights: function () {
    return this.props.document.noneditableHighlighedPagesForPageNo(this.props.filepage.number());
  },

  editableHighlight: function () {
    return this.props.document.editableHighlighedPageForPageNo(this.props.filepage.number());
  },

  hasEditableHighlight: function () {
    return this.editableHighlight() != undefined;
  },

  // TODO: should be replaced with only `change` event later.
  componentWillMount: function () {
    this.props.filepage.on("change:dragables", this.handleChange);
    this.props.image.onload = () => this.handleChange;
  },

  componentWillUnmount: function () {
    this.props.filepage.off("change:dragables", this.handleChange);
  },

  handleChange: function () {
    if (this.isMounted()) {
      this.forceUpdate();
    }
  },

  height: function () {
    if (!this.props.image.complete) {
      return 0;
    } else {
      return (this.props.width / this.props.image.width) * this.props.image.height;
    }
  },

  handleRemoveHighlighting: function () {
    this.props.onRemoveHighlighting(this.props.filepage.number());
  },

  handleNewHighlight: function (canvas) {
    this.props.onNewHighlight(this.props.filepage.number(), canvas);
  },

  renderFields: function () {
    const {filepage, document, width, signview} = this.props;
    const height = this.height();

    return _.map(document.allPlacements(), function (placement, index) {
      if (placement.page() === filepage.number()) {
        var field = placement.field();

        var args = {
          model: placement,
          pageWidth: width,
          pageHeight: height,
          signview: signview
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

        if (field.isRadioGroup()) {
          return <RadioButtonPlacementPlacedView key={index} {...args} />;
        }

        throw new Error("unknown field type");
      }
    });
  },

  render: function () {
    const {filepage, document, width, image} = this.props;

    const pageStyle = {
      width: width + "px"
    };

    const height = Math.floor(image.height * (width / DEFAULT_PIXEL_WIDTH));

    return (
      <div style={pageStyle} id={"page" + filepage.number()} className="pagediv">

        { /* if */ this.showHighlighting() &&
          _.map(this.noneditableHighlights(), function (hp) {
            return (
              <img
                onDragStart={(e) => { e.preventDefault(); }}
                style={{"position": "absolute"}}
                src={hp.file().downloadLink()}
              />
            );
          })
        }

        <img onDragStart={(e) => { e.preventDefault(); }} src={image.src} />

        { /* if */ this.canEditHighlighting() && (image.height > 0) &&
          <Highlight
            active={this.props.highlightingMode}
            removingHighlightingMode={this.props.removingHighlightingMode && this.hasEditableHighlight() }
            width={width}
            height={height}
            onNewHighlight={this.handleNewHighlight}
            baseImageURL={ this.hasEditableHighlight() ? this.editableHighlight().file().downloadLink() : undefined }
            onRemoveHighlighting={this.handleRemoveHighlighting}
          />
        }

        { /* if */ image.complete && !document.closed() &&
          this.renderFields()
        }
      </div>
    );
  }
});
