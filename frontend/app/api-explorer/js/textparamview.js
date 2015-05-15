
(function (window) {

window.TextParamView  = Backbone.View.extend({
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

    $(this.el).append($("<p/>").append($("<label  for='" + id + "' />").text(param.name())));
    var mainRow = $("<div class='row'>");
    $(this.el).append(mainRow);
    var input = $("<input id='" + id + "' type='text' class='form-control'/>");
    input.val(call.getParamValue(param));
    input.change(function () {
        call.setParamValue(param, input.val());
      });
    input.keypress(function (e) {
        if (e.which == 13) {
          setTimeout(function () {call.send();}, 10);
        }
      });

    var description = $("<p>");
    if(param.optional()) description.append($("<em>Optional.</em>"));
    description.append($("<p>").text(param.description()));

    mainRow.append($("<div class='col-xs-6'>").append(input))
           .append($("<div class='col-xs-6'>").append($("<p/>").append(description)));

    return this;
  }
});

})(window);
