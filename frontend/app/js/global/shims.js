// Shims
if (typeof String.prototype.startsWith != 'function') {
  String.prototype.startsWith = function (str){
    return this.slice(0, str.length) == str;
  };
}

if (typeof String.prototype.trim !== 'function') {
  String.prototype.trim = function() {
    return this.replace(/^\s+|\s+$/g, '');
  };
}

if (!window.console) {
  window.console = {};
}

if (!window.console.log) {
  window.console.log = function() {};
}

if (!window.console.warn) {
  window.console.warn = function() {};
}
