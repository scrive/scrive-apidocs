/** @jsx React.DOM */
/* Mixim for objects that can reload. Used with list, so you get reload functionality for free. Subobjects that will reload are checked only on refs list. */

define(['Backbone', 'legacy_code'], function() {

return {
  reload : function() {
    _.each(_.values(this.refs), function(c) {
      if (c.reload != undefined)
        c.reload();
    });
  }
};

});
