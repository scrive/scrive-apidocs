(function (window) {

window.CallSelector = Backbone.View.extend({
  initialize: function (args) {
    this.render();
  },
  callSelector: function () {
    var model = this.model;
    var select = $("<select id='call-selector' class='form-control'>");
    var availableCalls;
    if (model.apiVersion() == "v1") {
      availableCalls = APICalls.apiV1Calls(false);
    } else {
      availableCalls = APICalls.apiV2Calls(false);
    }
    availableCalls = _.sortBy(availableCalls, function (c) { return c.name(); });

    var mainCalls = _.filter(availableCalls, function (c) { return c.hasCategory("main"); });
    var fetchCalls = _.filter(availableCalls, function (c) { return c.hasCategory("fetch"); });
    var draftsCalls = _.filter(availableCalls, function (c) { return c.hasCategory("draft"); });
    var signingCalls = _.filter(availableCalls, function (c) { return c.hasCategory("signing"); });
    var otherCalls = _.filter(availableCalls, function (c) { return c.hasCategory("other"); });
    var attachmentCalls = _.filter(availableCalls, function (c) { return c.hasCategory("attachment"); });
    var v2UserRelatedCalls = _.filter(availableCalls, function (c) { return c.hasCategory("v2-user-related"); });

    select.append("<option disabled>-- Most common calls --</option>");
    _.each(mainCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", model.selectedApiCall().callPrototype() == c)
      );
    });

    select.append("<option disabled>-- Fetching documents --</option>");
    _.each(fetchCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", !c.hasCategory("main") && model.selectedApiCall().callPrototype() == c)
      );
    });

    select.append("<option disabled>-- Working with drafts --</option>");
    _.each(draftsCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", !c.hasCategory("main") && model.selectedApiCall().callPrototype() == c)
      );
    });

    select.append("<option disabled>-- Signing process --</option>");
    _.each(signingCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", !c.hasCategory("main") && model.selectedApiCall().callPrototype() == c)
        );
    });

    select.append("<option disabled>-- Other --</option>");
    _.each(otherCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", !c.hasCategory("main") && model.selectedApiCall().callPrototype() == c)
        );
    });

    select.append("<option disabled>-- Attachments --</option>");
    _.each(attachmentCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", !c.hasCategory("main") && model.selectedApiCall().callPrototype() == c)
        );
    });

    select.append("<option disabled>-- User related --</option>");
    _.each(v2UserRelatedCalls, function (c) {
      select.append(
        $("<option value='" + c.cid + "'>" + c.name() + "</option>")
          .attr("selected", !c.hasCategory("main") && model.selectedApiCall().callPrototype() == c)
        );
    });

    select.change(function () {
      var call = _.find(availableCalls, function (c) { return c.cid == select.val(); });
      model.setSelectedApiCall(call);
    });
    return select;
  },
  versionSelector: function () {
    var model = this.model;
    var select = $("<select id='version-selector' class='form-control'>");
    select.append($("<option value='v1'>API version 1</option>").attr("selected", model.apiVersion() == "v1"));
    select.append($("<option value='v2'>API version 2</option>").attr("selected", model.apiVersion() == "v2"));
    select.change(function () {
      model.changeAPIVersion(select.val());
    });
    return select;
  },
  render: function () {
    var self = this;
    var model = this.model;
    var selectCallCol = $("<p/>");
    var row = $("<div class='row form-horizontal'/>");
    row.append(
      $("<div class='col-xs-1 col-xs-offset-2 text-right'>")
        .append("<label for='call-selector' class='control-label'>API call</label>")
    );
    row.append($("<div class='col-xs-3'>").append(this.callSelector()));
    row.append($("<div class='col-xs-2'>").append(this.versionSelector()));
    selectCallCol.append(row);

    $(this.el).append(selectCallCol);
    return this;
  }
});

})(window);
