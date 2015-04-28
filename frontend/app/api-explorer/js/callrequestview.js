
(function (window) {

window.CallRequestView = Backbone.View.extend({
  initialize: function (args) {
    var self = this;
    _.bindAll(this, "render", "onScroll");
    $(window).bind("scroll", this.onScroll);
    this.render();
  },
  destroy: function () {
    $(window).unbind("scroll", this.onScroll);
    if (this.scrollpanel) {
      this.scrollpanel.remove();
    }
  },
  createScrollPanel: function () {
    if (this.scrollpanel != undefined) {
      this.scrollpanel.remove();
    }
    var model = this.model;
    var call = model.selectedApiCall();
    this.scrollpanel = $("<div class='panel panel-default panel-scroll' />");
    this.scrollpane.css("style", "position:fixed;top:-2px;margin-left:-1px")
    this.scrollpanel.css("width", $(this.el).width() + 2);
    var panelBody = $("<div class='panel-body'>");
    var mainRow = $("<div class='row'>");
    var descriptionHeader = $("<div class='col-xs-8'>");
    descriptionHeader.append($("<p/>").append($("<strong/>").text(call.method() + " " + call.sampleUrl())));
    descriptionHeader.append($("<p/>").text(call.description()));

    var sendButtonHeader = $("<div class='col-xs-4'>");
    sendButtonHeader.append($("<button class='btn btn-block btn-success' type='button'>Send</button>")
        .click(function () {
          call.send();
        })
      );
    panelBody.append(mainRow.append(descriptionHeader).append(sendButtonHeader));
    this.scrollpanel.append(panelBody);

    $(this.el).append(this.scrollpanel);
  },
  onScroll: function () {
    var limit = $(this.el).offset().top;
    if ($(window).scrollTop() >= limit + 2) {
      this.createScrollPanel()
    } else if (this.scrollpanel != undefined) {
      this.scrollpanel.remove();
    }

  },
  render: function () {
    var self = this;
    var model = this.model;
    var call = model.selectedApiCall();
    var panel = $(this.el);
    panel.children().detach();

    var panelBody = $("<div class='panel-body'>");
    var mainRow = $("<div class='row'>");
    var descriptionHeader = $("<div class='col-xs-8 request-description'>");
    descriptionHeader.append($("<p/>").append($("<strong/>").text(call.method() + " " + call.sampleUrl())));
    descriptionHeader.append($("<p/>").text(call.description()));

    var sendButtonHeader = $("<div class='col-xs-4'>");
    sendButtonHeader.append($("<button class='btn btn-block btn-success' type='button'>Send</button>")
        .click(function () {
          call.send();
        })
      );
    panelBody.append(mainRow.append(descriptionHeader).append(sendButtonHeader));
    panel.append(panelBody);

    if (call.params().length > 0) {
      var paramList = $("<ul class='list-group request-params'/>");

      panel.append(paramList);
      _.each(call.params(), function (p) {
        var paramView;
        if (p.type() == "text") {
          paramView = new TextParamView({param: p, call: call});
        } else if (p.type() == "bool") {
          paramView = new BoolParamView({param: p, call: call});
        } else if (p.type() == "json") {
          paramView = new JSONParamView({param: p, call: call});
        } else if (p.type() == "file") {
          paramView = new FileParamView({param: p, call: call});
        }

        paramList.append(
          $("<li class='list-group-item'/>").append(paramView.el)
        );
      })
    }
    this.onScroll();
    return this;
  }
});

})(window);
