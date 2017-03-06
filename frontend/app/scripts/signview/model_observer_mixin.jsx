var _ = require("underscore");

var ModelObserverMixin = {
  hasChanges: function (model, fields) {
    if (model.changedAttributes()) {
      var changes = _.intersection(
        _.keys(model.changedAttributes()), fields
      );

      return (changes.length > 0);
    }

    return false;
  }
};

module.exports = ModelObserverMixin;
