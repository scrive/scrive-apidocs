define(["Backbone", "legacy_code"], function (Backbone) {
  return Backbone.Model.extend({
    signatories: function () {
      var signatories = this.document().signatories();
      var current = _.find  (signatories, function (s) { return s.current(); });
      var others  = _.filter(signatories, function (s) { return !s.current(); });
      var sigs = _.compact([current].concat(others));
      _.filter(sigs, function (s) { return s.signs(); });

      return sigs;
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
