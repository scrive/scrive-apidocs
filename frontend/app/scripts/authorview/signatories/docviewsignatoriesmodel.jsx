define(["Backbone", "legacy_code"], function (Backbone) {
  return Backbone.Model.extend({
    defaults: function () {
      return {onAction: function () {}};
    },

    signatories: function () {
      var signatories = this.document().signatories();
      var current = _.find  (signatories, function (s) { return s.current(); });
      var others  = _.filter(signatories, function (s) { return !s.current(); });
      var sigs = _.compact([current].concat(others));

      return sigs;
    },

    onAction: function () {
      return this.get("onAction");
    },

    hasAutomaticReminder: function () {
      var hasNotSigned = _.any(this.document().signatories(), function (s) {
        return s.signs() && !s.hasSigned() && !s.padDelivery();
      });

      return this.document().pending()
        && (this.document().timeouttime().diffDays() > 0 || this.document().autoremindtime() != undefined)
        && hasNotSigned
        && this.document().currentViewerIsAuthor();
    },

    document: function () {
      return this.get("document");
    },

    hasList: function () {
      return this.signatories().length > 2;
    },

    isSingleSignatory: function () {
      return this.signatories().length == 1;
    }
  });
});
