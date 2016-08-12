import $ from "jquery";
import _ from "underscore";
import Backbone from "backbone";

function sortByView (tasks) {
  var newList = tasks.slice(0);

  newList.sort(function (t1, t2) {
    var offset1 = t1.el().offset();
    var offset2 = t2.el().offset();
    var height1 = t1.el().outerHeight();
    var height2 = t2.el().outerHeight();
    var verticalDiff = (offset1.top + height1) - (offset2.top + height2);
    var horizontalDiff = offset1.left - offset2.left;
    return (verticalDiff === 0 ? horizontalDiff : verticalDiff);
  });

  return newList;
}

module.exports = Backbone.Model.extend({
  defaults: {
    list: [],
    active: null
  },

  active: function () {
    return this.get("active");
  },

  list: function () {
    return this.get("list");
  },

  incomplete: function () {
    return _.filter(this.list(), (task) => !task.isComplete());
  },

  add: function (task) {
    var list = this.list();
    var newList = list.concat([task]);
    this.set({list: sortByView(newList)});
    this.listenTo(task, "change", this.setActive);
    this.setActive();
  },

  remove: function (task) {
    var list = this.list();
    var newList = _.without(list, task);
    this.set({list: sortByView(newList)});
    this.stopListening(task, "change", this.setActive);
    this.setActive();
  },

  setActive: function () {
    var active = this.active();
    var wasTask = active !== null;
    var incomplete = this.incomplete();

    if (incomplete.length === 0) {
      if (wasTask) {
        active.onDeactivate();
      }

      this.set({active: null});
    } else {
      var newActive = incomplete[0];
      if (newActive !== active) {
        if (wasTask) {
          active.onDeactivate();
        }

        newActive.onActivate();

        this.set({active: newActive});
      }
    }
  },

  triggerOnActivate: function () {
    var active = this.active();

    if (active) {
      active.forceOnActivate();
    }
  }
});
