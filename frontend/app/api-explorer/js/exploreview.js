
(function (window) {

window.ApiDemoExploreView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");
    this.render();
  },
  header: function () {
    var self = this;
    var model = this.model;
    var header = $("<div class='navbar'/>");
    var headerContainer = $("<div class='container'/>");
    var headerRightBarList = $("<ul class='navbar-nav list-inline'/>");
    headerRightBarList.append($("<li><a class='active' href='#'><b>API explorer</b></a></li>"))
                      .append($("<li><a href='/en/api'>API documentation</a> </li>"));

    if (!model.oauth().isSetUp()) {
      headerRightBarList.append($("<li><a>Authorise</a></li>").click(function () {
        model.setMode("authorize");
        window.location.hash = "#" + model.oauth().mode();
      }));
    } else {
      headerRightBarList.append($("<li><a>Clear authorisation</a></li>").click(function () {
        model.oauth().clear();
      }));
    }
    headerContainer.append("<div class='navbar-header'><img src='img/logo.png/'/><div>");
    headerContainer.append($("<div class='navbar-right'/>").append(headerRightBarList));
    header.append(headerContainer);
    return header;
  },
  cleanup: function () {
    if (this.requestView) {
      this.requestView.destroy();
    }
    if (this.responseView) {
      this.responseView.destroy();
    }
  },
  render: function () {
    var self = this;
    self.cleanup();
    var model = this.model;
    var oauth = model.oauth();
    var container = $("<div class='container'/>");
    this.requestView = new CallRequestView({model: model, el: $("<div class='panel panel-default'/>")});
    this.responseView = new CallResponseView({model: model, el: $("<div class='panel panel-default'/>")});
    container.append(new CallSelector({model: model, el: $("<div class='row calls-row'/>")}).el);
    container.append($("<div class='row'/>")
      .append($("<div class='col-xs-6'/>").append(this.requestView.el))
      .append($("<div class='col-xs-6'/>").append(this.responseView.el))
    );

    $(this.el).addClass("main").children().detach();
    $(this.el).append(self.header()).append(container);
    return this;
  }
});

})(window);
