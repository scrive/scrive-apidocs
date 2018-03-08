import $ from "jquery";
import _ from "underscore";
import Backbone from "backbone";

function sortByView (tasks) {
  var newList = tasks.slice(0);

  newList.sort(function (t1, t2) {
    // Hack to make sure that 'sign' task will be at the end, even if during
    // some race condition (while page loads) field tasks will be 'bigger'
    // according to offset calculations (below the sign task from the top)
    // See https://scrive.fogbugz.com/f/cases/2656/
    if (t1.isSignTask()) {
      return 1;
    } else if (t2.isSignTask()) {
      return -1;
    }

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
    active: []
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
    var wasTask = active.length > 0;
    var incomplete = this.incomplete();

    var triggerOnDeactivate = function () {
      _.forEach(active, function (item) {
        item.onDeactivate();
      });
    };

    if (incomplete.length === 0) {
      if (wasTask) {
        triggerOnDeactivate();
      }

      this.set({active: []});
    } else {
      var nextTask = incomplete[0];
      var newActive = [];

      if (nextTask.isFieldTask() && nextTask.field().isRadioGroup()) {
        _.forEach(incomplete, function (item) {
          if (item.field() == nextTask.field()) {
            newActive.push(item);
          }
        });
      } else if (nextTask.isConsentQuestion()) {
        _.forEach(incomplete, function (item) {
          if (item.consentQuestion() == nextTask.consentQuestion()) {
            newActive.push(item);
          }
        });
      } else {
        newActive.push(nextTask);
      }

      if (wasTask) {
        triggerOnDeactivate();
      }

      _.forEach(newActive, function (item) {
        item.onActivate();
      });

      this.set({active: newActive});
    }
  },

  triggerOnActivate: function () {
    var active = this.active();

    if (active.length > 0) {
      _.forEach(active, function (item) {
        item.forceOnActivate();
      });
    }
  }
});
