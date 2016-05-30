
(function (window) {

window.CredentialsView = Backbone.View.extend({
  initialize: function (args) {
    this.render();
  },
  render: function () {
    var self = this;
    var model = this.model;
    var oauth = model.oauth();
    var credentiasBox = $(this.el).empty();
    var credentiasRow = $("<div class='row'/>");
    var credentiasCol1 = $("<div class='col-xs-3'/>");
    var credentiasCol2 = $("<div class='col-xs-3 '/>");
    var credentiasCol3 = $("<div class='col-xs-3 '/>");
    var credentiasCol4 = $("<div class='col-xs-3 '/>");
    credentiasCol1
      .append("<p><b>Client credentials identifier</b><BR/><span>" + oauth.consumer_key() + "</span></p>");
    credentiasCol2
      .append("<p><b>Client credentials secret</b><BR/><span>" + oauth.client_shared_secret() + "</span></p>");
    credentiasCol3
      .append("<p><b>Token credentials identifier</b><BR/><span>" + oauth.final_token() + "</span></p>");
    credentiasCol4
      .append("<p><b>Token credentials secret</b><BR/><span>" + oauth.final_token_secret() + "</span></p>");
    credentiasBox.append(credentiasRow
      .append(credentiasCol1)
      .append(credentiasCol2)
      .append(credentiasCol3)
      .append(credentiasCol4)
    );
    return this;
  }
});

})(window);
