var React = require("react");
var TaskList = require("../navigation/task_list");

  module.exports = {
    contextTypes: {
      taskList: React.PropTypes.instanceOf(TaskList),
    },

    hasTaskContext: function () {
      return typeof this.context.taskList === "object";
    },

    addTasks: function () {
      if (!this.hasTaskContext()) {
        return;
      }

      var self = this;

      if (typeof self.createTasks !== "function") {
        throw new Error("TaskMixin requires createTasks method to be defined");
      }

      var tasks = self.createTasks();

      if (tasks instanceof Array) {
        self._tasks = tasks;

        tasks.forEach(function (task) {
          self.context.taskList.add(task);
        });
      }
    },

    removeTasks: function () {
      if (!this.hasTaskContext()) {
        return;
      }

      var self = this;
      var tasks = self._tasks;

      if (tasks instanceof Array) {
        tasks.forEach(function (task) {
          self.context.taskList.remove(task);
        });
      }
    },

    forceUpdateTasks: function () {
      this.removeTasks();
      this.addTasks();
    },

    componentDidUpdate: function () {
      var self = this;
      var tasks = self._tasks;

      if (tasks instanceof Array) {
        tasks.forEach(function (task) {
          task.update();
        });
      }
    },

    componentDidMount: function () {
      this.addTasks();
    },

    componentWillUnmount: function () {
      this.removeTasks();
    }
  };
