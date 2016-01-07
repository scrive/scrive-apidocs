
define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "designview/fileview/filepageview"],
  function (legacy_code, _, Backbone, React, BackboneMixin, FilePageView) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(File).isRequired,
      pixelWidth: React.PropTypes.number.isRequired
    },

    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    getInitialState: function () {
      return {images: [], visibleCoordinates: false, top: 0, width: 0, left: 0, height: 0};
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (this.props.model.pages().length !== this.state.images.length) {
        this.updateImages();
      }
    },

    showCoordinateAxes: function () {
      this.setState({"visibleCoordinates": true});
    },

    hideCoordinateAxes: function () {
      this.setState({"visibleCoordinates": false});
    },

    moveCoordinateAxes: function (helper, verticalOffset, xAxisOffset, yAxisOffset) {
      var self = this;
      if (this.isMounted()) {
        var offset =  $(this.refs.pages.getDOMNode()).offset();
        var top = helper.offset().top - offset.top + helper.outerHeight() + verticalOffset;
        top -= yAxisOffset || 0;
        var left = helper.offset().left - offset.left;
        left += xAxisOffset || 0;
        var height = $(this.getDOMNode()).height();
        var width = $(this.getDOMNode()).width();
        this.setState({top: top, width: width, left: left, height: height});
      }
    },

    ready: function () {
      var model = this.props.model;
      return model.ready() && model.pages().length > 0 &&
        this.state.images.length === model.pages().length &&
        _.all(this.state.images, function (img) { return img.complete; });
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
        if (BrowserInfo.isIE8orLower()) {
          img.detachEvent("onload", self.handleLoad);
        } else {
          img.removeEventListener("load", self.handleLoad);
        }
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
        if (BrowserInfo.isIE8orLower()) {
          img.attachEvent("onload", callback);
        } else {
          img.addEventListener("load", callback);
        }
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
      }
    },

    openTypeSetterFor: function (placement) {
      if (this.isMounted()) {
        _.each(this.refs, function (v) {
          if (v.openTypeSetterOnThisPageFor) {
            v.openTypeSetterOnThisPageFor(placement);
          }
        });
      }
    },

    closeAllTypeSetters: function () {
      if (this.isMounted()) {
        _.each(this.refs, function (v) {
          if (v.closeAllTypeSettersOnThisPage) {
            v.closeAllTypeSettersOnThisPage();
          }
        });
      }
    },

    renderPage: function (page, index) {
      var image = this.state.images[index];
      if (!image) {
        return <span key={index} />;
      }
      var imageWidth = image.width;
      var imageHeight = image.height;
      var width = imageWidth;
      var height = imageHeight;
      return (
        <FilePageView
          ref={"page-" + index}
          key={index}
          model={page}
          imageSrc={image.src}
          imageComplete={image.complete}
          imageWidth={width}
          imageHeight={height}
          showCoordinateAxes={this.showCoordinateAxes}
          hideCoordinateAxes={this.hideCoordinateAxes}
          moveCoordinateAxes={this.moveCoordinateAxes}
          closeAllTypeSetters={this.closeAllTypeSetters}
        />
      );
    },

    render: function () {
      var self = this;
      var model = this.props.model;
      return (
        <div className="design-view-document-pages">
          <div className="document-pages" ref="pages">
            {/* if */ !model.ready() &&
              <div className="waiting4page"/>
            }
            {/* else */ model.ready() &&
              _.map(model.pages(), this.renderPage)
            }
            {/* if */ self.state.visibleCoordinates &&
              <div>
                <div
                  className="vline"
                  style={{
                    left: this.state.left + "px",
                    height: this.state.height + "px"
                  }}
                />
                <div
                  className="hline"
                  style={{
                    top: this.state.top + "px",
                    width: this.state.width + "px"
                  }}
                />
              </div>
            }
          </div>
        </div>
      );
    }
  });
});
