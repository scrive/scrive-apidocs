
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

    var sendEmptyMain = $("<div class='checkbox'>");
    if (param.optionToSendEmpty()) {
      var sendEmptyInput = $("<input type='checkbox' class=''/>");
      sendEmptyInput.prop("checked", false);
      call.setParamSendEmpty(param, sendEmptyInput.is(":checked"));
      sendEmptyInput.change(function () {
        call.setParamSendEmpty(param, sendEmptyInput.is(":checked"));
      });

      sendEmptyMain.append($("<label>")
        .append(sendEmptyInput)
        .append("Send parameter even if empty")
      );
    }

    var multifile =  call.getParamValue(param);
    var slavesWithFiles = _.filter(multifile.slaves, function (s) { return s != undefined && s != multifile.current; });
    if (slavesWithFiles.length < param.limit()) {

      var description = $("<p>");
      if (param.optional()) {
        description.append($("<em>Optional.</em>"));
      }
      description.append($("<p>").text(param.description()));

      var inputColumn = $("<div class='col-xs-6'>").append($("<div class='form-group'/>").append(multifile.current));
      if (param.optionToSendEmpty()) {
        inputColumn.append(sendEmptyMain);
      }
      mainRow.append(inputColumn)
             .append($("<div class='col-xs-6'>").append($("<p/>").append(description)));
    }

    if (slavesWithFiles.length  > 0) {
      var tbody =  $("<tbody/>");
      $(this.el).append($("<table class='table table-striped table-bordered'>").append(tbody));
      var remover = function (i) {
        return function () {
          multifile.slaves[i] = null;
          self.render();
        };
      };
      var rowCount = 0;
      for (var i = 0; i < multifile.slaves.length; i++) {
        if (multifile.slaves[i] != undefined && multifile.slaves[i] != multifile.current) {
          var row = $("<tr/>");
          row.append($("<td/>").text(param.argName(rowCount)));
          row.append($("<td/>").text((multifile.slaves[i].value || "").match(/[^/\\]+$/gi)[0]));
          row.append($("<td class='text-right'/>")
             .append($("<button class='btn btn-xs btn-default' type='button'>Remove</button>").click(remover(i)))
            );
          tbody.append(row);
        } else {
          rowCount--;
        }
        rowCount++;
      }
    }
    return this;
  }
});

})(window);
