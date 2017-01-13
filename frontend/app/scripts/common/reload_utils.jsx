/* Reloading utils */

var Submit = require("../../js/submits.js").Submit;

module.exports = {

  "reloadWithScrollReset": function () {
    new Submit().send(); // Same as window.location.reload(), but will reset scrolling
  }
};
