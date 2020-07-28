var React = require("react");
var _ = require("underscore");

import {toLessInteropLoader} from "../../common/less_utils.jsx";
import DocumentPagesLess_ from "!@hon2a/less-vars-loader!../../../less/document-pages.less";
const DocumentPagesLess = toLessInteropLoader(DocumentPagesLess_);

var CheckboxPlacementView = require("./checkboxplacementview");
var RadioButtonPlacementView = require("./radiobuttonplacementview");
var SignaturePlacementView = require("./signatureplacementview");
var TextPlacementView = require("./textplacementview");

var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FilePage = require("../../../js/files.js").FilePage;
var HighlightedPage = require("../../../js/highlightedpage.js").HighlightedPage;

var PlacementsContainerView = React.createClass({
  propTypes: {
    placements: React.PropTypes.arrayOf(React.PropTypes.instanceOf(FieldPlacement)).isRequired,
    pageNumber: React.PropTypes.number.isRequired
  },
  getInitialState: function () {
    return {
      pageWidth: 0,
      pageHeight: 0
    };
  },
  render: function () {
    var self = this;

    var pagePlacements = _.filter(this.props.placements, function (item) {
      return (item.page() == self.props.pageNumber);
    });

    return (
      <div>
        {
          _.map(pagePlacements, function (item, index) {
            var viewArgs = {
              key: index,
              model: item,
              pageWidth: self.state.pageWidth,
              pageHeight: self.state.pageHeight
            };

            if (item.field().isSignature()) {
              return <SignaturePlacementView {...viewArgs} />;
            } else if (item.field().isCheckbox()) {
              return <CheckboxPlacementView {...viewArgs} />;
            } else if (item.field().isText()) {
              return <TextPlacementView {...viewArgs} />;
            } else if (item.field().isRadioGroup()) {
              return <RadioButtonPlacementView {...viewArgs} />;
            } else {
              throw new Error("unknown field type: " + item.field().type());
            }
          })
        }
      </div>
    );
  }
});

module.exports = React.createClass({
  propTypes: {
    page: React.PropTypes.instanceOf(FilePage).isRequired,
    highlightedPages: React.PropTypes.instanceOf(HighlightedPage).isRequired,
    onReady: React.PropTypes.func.isRequired
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevState.imageReady != this.state.imageReady) {
      this.handleReadyStateChange();

      if (this.props.onReady) {
        this.props.onReady(this);
      }
    }
  },
  getInitialState: function () {
    return {
      imageReady: false
    };
  },
  imageSrc: function () {
    var srcParts = [
      "/pages/", this.props.page.file().fileid(), "/",
      this.props.page.number(),
      this.props.page.file().queryPart({
        "pixelwidth": this.props.page.width()
      })
    ];

    return srcParts.join("");
  },
  ready: function () {
    return this.state.imageReady;
  },
  onImageLoad: function () {
    this.setState({imageReady: true});
  },
  handleReadyStateChange: function () {
    this.updateImageSize();
    this.updatePlacementsContainerState();
  },
  updatePlacementsContainerState: function () {
    if (this.refs.placementsContainer) {
      this.refs.placementsContainer.setState({
        pageWidth: this.props.page.width(),
        pageHeight: this.props.page.height()
      });
    }
  },
  updateImageSize: function () {
    if (this.state.imageReady) {
      var imageNode = React.findDOMNode(this.refs.pagejpg);

      var pageWidth = DocumentPagesLess.pageDivWidth;
      var pageHeight = imageNode.height;

      if (imageNode.width != pageWidth) {
        pageHeight = (pageWidth / imageNode.width) * imageNode.height;
      }

      this.props.page.setSize(pageWidth, pageHeight);
    }
  },
  render: function () {
    var pageId = "page" + this.props.page.number();

    return (
      <div id={pageId} className="pagediv">
        { _.map(this.props.highlightedPages, function (hp) {
            return (
              <img style={{"position": "absolute"}} src={hp.file().downloadLink()} />
            );
          })
        }

        <img
          className="pagejpg"
          ref="pagejpg"
          src={this.imageSrc()}
          onLoad={this.onImageLoad}
        />

        { /* if */ (!this.props.page.file().document().closed()) &&
          <PlacementsContainerView
            ref="placementsContainer"
            pageNumber={this.props.page.number()}
            placements={this.props.page.placements()}
          />
        }
      </div>
    );
  }
});
