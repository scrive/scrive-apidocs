var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var FilePageView = require("./filepageview");
var File = require("../../../js/files.js").File;
var $ = require("jquery");
var classNames = require("classnames");
var vars = require("../../../less/signview/vars.less");
var viewSize = require("../viewsize");
var PageViewer = require("../pageviewer/pageviewer");

  module.exports = React.createClass({
    displayName: "FileView",

    propTypes: {
      model: React.PropTypes.instanceOf(File).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      pixelWidth: React.PropTypes.number.isRequired,
      showOverlay: React.PropTypes.bool.isRequired,
      showArrow: React.PropTypes.bool.isRequired
    },

    mixins: [BackboneMixin.BackboneMixin, React.addons.PureRenderMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    getInitialState: function () {
      return {images: []};
    },

    componentWillMount: function () {
      this.props.model.view = this;
      $(window).on("resize", this.handleResize);
    },

    componentWillUnmount: function () {
      this.props.model.view = null;
      $(window).off("resize", this.handleResize);
    },

    handleResize: function () {
      this.forceUpdate();
    },

    componentDidUpdate: function (prevProps, prevState) {
      var signview = this.props.signview;

      if (this.props.model.pages().length !== this.state.images.length) {
        this.updateImages();
      }
    },

    zoomToPoint: function (zoomPoint, zoom) {
      this.refs.viewer.zoomToPoint(zoomPoint, zoom);
    },

    ready: function () {
      var model = this.props.model;
      var ready = model.ready() && model.pages().length > 0 &&
        this.state.images.length === model.pages().length &&
        _.all(this.state.images, function (img) { return img.complete; });
      return ready;
    },

    readyFirstPage: function () {
      return this.state.images.length > 0 && this.state.images[0].complete;
    },

    updateImages: function () {
      var self = this;
      var file = self.props.model;
      var fileid = file.fileid();
      var pixelWidth = this.props.pixelWidth;

      _.each(self.state.images, function (img) {
        img.removeEventListener("load", self.handleLoad);
      });

      var images = _.map(file.pages(), function (page, index) {
        var pagelink = "/pages/" + fileid  + "/" + page.number() + file.queryPart({"pixelwidth": pixelWidth});
        var img = new Image();
        var callback = function () {
          if (!img.complete) {
            setTimeout(callback, 100);
          } else {
            self.handleLoad(index);
          }
        };
        img.addEventListener("load", callback);
        img.src = pagelink;
        return img;
      });

      self.setState({images: images});
    },

    handleLoad: function (index) {
      if (index === 0) {
        this.props.model.trigger("FirstPageReady");
      }

      if (this.ready()) {
        this.forceUpdate();
        this.props.model.trigger("view:ready");
        this.props.model.trigger("change");
        this.props.signview.trigger("change");
      }
    },

    renderPage: function (page, index) {
      var image = this.state.images[index];
      var signview = this.props.signview;

      if (!image) {
        return <span key={index} />;
      }

      return (
        <FilePageView
          key={"file_" + index}
          model={page}
          signview={signview}
          image={image}
        />
      );
    },

    render: function () {
      var file = this.props.model;

      return (
        <PageViewer
          ref="viewer"
          ready={file.ready()}
          showArrow={this.props.showArrow}
          showOverlay={this.props.showOverlay}
        >
          {_.map(file.pages(), this.renderPage)}
        </PageViewer>
      );
    }
  });
