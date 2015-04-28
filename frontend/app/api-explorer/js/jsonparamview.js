
(function (window) {

window.JSONParamView  = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");
    this.param = args.param;
    this.call = args.call;
    this.render();
  },
  render: function () {
    var self = this;
    var param = this.param;
    var call = this.call;
    var id = call.urlHash() + "-" + param.argName();
    var mainRow = $("<div class='row'>");
    mainRow.append(
             $("<div class='col-xs-6'>").append($("<p/>").append($("<label for='" + id + "' />").text(param.name())))
            )
           .append(
             $("<div class='col-xs-6'>").append($("<p/>").text(param.description()))
           );
    $(this.el).append(mainRow);

    var textarea = $("<textarea  id='" + id + "' class='form-control'/>");
    textarea.autoGrow();
    // This needs to be done in timeout since textarea (or one of it's parents) is not yet appended to body
    setTimeout(function () {textarea.autoGrow();}, 1);
    textarea.val(call.getParamValue(param));
    textarea.bind("paste", function () {
        setTimeout(function () {textarea.autoGrow();}, 1);
      });
    textarea.change(function () {
        call.setParamValue(param, textarea.val());
      });
    $(this.el).append(textarea);

    return this;
  }
});

})(window);
