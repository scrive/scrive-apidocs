define(["React"], function (React) {
  return {
    contextTypes: {
      addTask: React.PropTypes.func.isRequired,
      removeTask: React.PropTypes.func.isRequired
    },

    componentDidMount: function () {
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

    componentDidUpdate: function () {
      var self = this;
      var tasks = self._tasks;

      if (tasks instanceof Array) {
        tasks.forEach(function (task) {
          task.update();
        });
      }
    },

    componentWillUnmount: function () {
      var self = this;
      var tasks = self._tasks;

      if (tasks instanceof Array) {
        tasks.forEach(function (task) {
          self.context.removeTask(task);
        });
      }
    }
  };
});
