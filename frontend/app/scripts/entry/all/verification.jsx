var FileVerifier = require("../../../js/fileverifier.js").FileVerifier;
var $ = require("jquery");

$(function () {
  $(".verificationContainer").append(FileVerifier.init().view.el);
});
