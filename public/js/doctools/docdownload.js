(function(window){
  
window.DocumentDownloadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  render: function() {
    $(this.el).empty();

    if (this.model.mainfile() == undefined) {
      return this;
    }

    var link = $("<a target='_blank' />");
    link.attr("href", this.model.mainfile().downloadLinkForMainFile(this.model.title()));
    link.append($("<div class='float-left icon' />"));
    link.append($("<div class='float-left label' />").text(localization.downloadPDF));
    link.append($("<div class='float-left docname' />").text(this.model.title() + ".pdf"));
    link.append($("<div class='clearfix' />"));

    $(this.el).append($("<div class='download' />").append(link));

    return this;
  }
});

})(window);
