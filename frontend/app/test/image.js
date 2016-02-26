var Image = function () {
    this.shim = true;

    this.src = "";
    this.complete = false;
    this.width = 0;
    this.height = 0;
    this.listeners = {};

    setTimeout(function () {
      this.complete = true;
      this.width = 800;
      this.height = 600;

      if (this.listeners.load) {
        this.listeners.load();
      }
    }.bind(this), 200);
  };

  Image.prototype.addEventListener = function (type, fn) {
    this.listeners[type] = fn;
  };

  Image.prototype.removeEventListener = function (type, fn) {
    delete this.listeners[type];
  };

  module.exports = Image;
