
(function (window) {

window.FileParamView  = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");
    this.param = args.param;
    this.call = args.call;
    this.param.bind("files-changed", this.render);
    this.render();
  },
  render: function () {
    $(this.el).children().detach();
    var self = this;
    var param = this.param;
    var call = this.call;
    $(this.el).append($("<p/>").append($("<label/>").text(param.name())));
    var mainRow = $("<div class='row'>");
    $(this.el).append(mainRow);

    var multifile =  call.getParamValue(param);
    slavesWithFiles = _.filter(multifile.slaves, function (s) { return s != undefined && s != multifile.current;});
    if (slavesWithFiles.length < param.limit()) {
      mainRow.append($("<div class='col-xs-6'>").append($("<div class='form-group'/>").append(multifile.current)))
             .append($("<div class='col-xs-6'>").append($("<p/>").text(param.description())));
    }

    if (slavesWithFiles.length  > 0) {
      var tbody =  $("<tbody/>");
      $(this.el).append($("<table class='table table-striped table-bordered'>").append(tbody));
      for (var i = 0; i < multifile.slaves.length; i++) {
        if (multifile.slaves[i] != undefined && multifile.slaves[i] != multifile.current) {
          var id = i;
          var row = $("<tr/>");
          row.append($("<td/>").text(param.argName(i)));
          row.append($("<td/>").text((multifile.slaves[i].value || "").match(/[^\/\\]+$/gi)[0]));
          row.append($("<td class='text-right'/>")
             .append($("<button class='btn btn-xs btn-default' type='button'>Remove</button>")
                  .click(function () {
                    multifile.slaves[id] = null;
                    self.render();
                  })
               )
            );
          tbody.append(row);
        }
      };
    }
    return this;
  }
});

})(window);
