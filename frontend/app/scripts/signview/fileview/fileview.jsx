define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "signview/fileview/filepageview"],
  function (legacy_code, _, Backbone, React, BackboneMixin, FilePageView) {
  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(File).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      arrow: React.PropTypes.func.isRequired,
      onReady: React.PropTypes.func
    },

    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    getInitialState: function () {
      return {images: []};
    },

    componentWillMount: function () {
      var model = this.props.model;

      model.view = this;
      model.fetch({
        data: {signatoryid: model.signatoryid()},
        processData: true,
        cache: false
      });
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (this.props.model.pages().length !== this.state.images.length) {
        this.updateImages();
      }
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

      _.each(self.state.images, function (img) {
        if (BrowserInfo.isIE8orLower()) {
          img.detachEvent("onload", self.handleLoad);
        } else {
          img.removeEventListener("load", self.handleLoad);
        }
      });

      var images = _.map(file.pages(), function (page, index) {
        var pagelink = "/pages/" + fileid  + "/" + page.number() + file.queryPart({"pixelwidth": page.width()});
        var img = new Image();
        var callback = function () {
          if (!img.complete) {
            setTimeout(callback, 100);
          } else {
            self.handleLoad(index)
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
        if (this.props.onReady) {
          this.props.onReady();
        }
      }
    },

    renderPage: function (page, index) {
      var image = this.state.images[index];

      if (!image) {
        return <span key={index} />;
      }

      return (
        <FilePageView
          key={index}
          model={page}
          arrow={this.props.arrow}
          signview={this.props.signview}
          imageSrc={image.src}
          imageComplete={image.complete}
          imageWidth={image.width}
          imageHeight={image.height}
        />
      );
    },

    render: function () {
      var file = this.props.model;

      return (
        <div className="document-pages">
          {/* if */ !file.ready() &&
            <div className="waiting4page" />
          }
          {/* else */ file.ready() &&
            _.map(file.pages(), this.renderPage)
          }
        </div>
      );
    }
  });
});
