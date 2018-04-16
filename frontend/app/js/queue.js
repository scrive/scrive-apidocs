var Backbone = require("backbone");

/* This object manages a queue of items to be processed in turn.
 *
 * It should be initialised with a function to process items and optionally a
 * list of initial items. This `processItem` function takes an item and a
 * callback function to trigger the processing of the next item in the queue.
 *
 * One can add items to an already created queue with pushItems(items).
 *
 * Every time the queue is emptied, onEmpty() is called.
 */

exports.Queue = Backbone.Model.extend({
  initialize: function(args) {
    var items = args.items ? args.items : [];
    this.set({
      items: items,
      processItem: args.processItem,
      total: items.length,
      onEmpty: args.onEmpty ? args.onEmpty : function () {},
      busy: false,
      currentItem: undefined
    });
    this.processNextItem();
  },

  isBusy: function() {
    return this.get("busy");
  },

  processNextItem: function() {
    var self = this;
    if(!self.isBusy()) {
      var item = self.get("items").shift();
      if(item) {
        self.set({
          busy: true,
          currentItem: item
        });

        self.get("processItem")(item, function() {
          self.set({
            busy: false,
            currentItem: undefined
          });

          if(self.isEmpty()) {
            self.onEmpty();
          } else {
            self.processNextItem();
          }
        });
      }
    }
  },

  pushItems: function(newItems) {
    var oldItems = this.get("items");
    this.set({
      items: oldItems.concat(newItems),
      total: this.get("total") + newItems.length
    });
    this.processNextItem();
  },

  pushItem: function(item) {
    this.pushItems([item]);
  },

  total: function() {
    return this.get("total");
  },

  remaining: function() {
    return this.get("items").length + (this.isBusy() ? 1 : 0);
  },

  processed: function() {
    return this.total() - this.remaining();
  },

  isEmpty: function() {
    return this.remaining() == 0;
  },

  onEmpty: function() {
    return this.get("onEmpty")();
  },

  currentItem: function() {
    return this.get("currentItem");
  }
});
