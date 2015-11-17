define(["React"], function (React) {
  return {
    contextTypes: {
      addTask: React.PropTypes.func,
      removeTask: React.PropTypes.func
    },

    hasTaskContext: function () {
      return typeof this.context.addTask === "function" && typeof this.context.removeTask === "function";
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
          self.context.addTask(task);
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
          self.context.removeTask(task);
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
});
