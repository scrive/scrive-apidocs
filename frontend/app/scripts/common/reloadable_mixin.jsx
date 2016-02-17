var _ = require("underscore");
/* Mixim for objects that can reload. Used with list, so you get reload functionality for free. Subobjects that will reload are checked only on refs list. */


module.exports = {
  reload : function() {
    _.each(_.values(this.refs), function(c) {
      if (c.reload != undefined)
        c.reload();
    });
  }
};
