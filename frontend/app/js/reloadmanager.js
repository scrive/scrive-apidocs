define(['legacy_code'], function() {

var ReloadManagerClass = function () {
  this.fns = [];
  this.blocking = true;
};

// private method that updates onbeforeunload
ReloadManagerClass.prototype._updateBlocking = function () {
  if (this.fns.length > 0 && this.blocking) {
    window.onbeforeunload = this.fns[0];
  } else {
    window.onbeforeunload = function () { };
  }
};

// stop blocking anything
ReloadManagerClass.prototype.stopBlocking = function () {
  this.blocking = false;
  this._updateBlocking();
};

// start blocking if blocking was turned off
ReloadManagerClass.prototype.startBlocking = function () {
  this.blocking = true;
  this._updateBlocking();
};

// add a function to the top of the blocking queue
ReloadManagerClass.prototype.pushBlock = function (fn) {
  this.fns.unshift(fn);
  this._updateBlocking();
};

// remove a function from blocking queue
ReloadManagerClass.prototype.popBlock = function (fn) {
  this.fns = _.without(this.fns, fn);
  this._updateBlocking();
};

window.ReloadManager = new ReloadManagerClass();

});
