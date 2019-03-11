var Submit = require("../../js/submits.js").Submit;

var expose = {

  checkPassword : function(password, successCallback, failureCallback) {
      new Submit({
        method: "POST",
        url: "/api/frontend/checkpassword",
        password: password,
        ajaxsuccess: function (resp, s, xhr) {
          if (resp.valid) {
            successCallback();
          } else {
            failureCallback();
          }
        },
        ajaxerror: failureCallback
      }).sendAjax();
  }
};

module.exports = expose;
