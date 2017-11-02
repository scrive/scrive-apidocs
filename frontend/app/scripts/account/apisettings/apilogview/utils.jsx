var classNames = require("classnames");

var statusClassName = function (isSuccessful) {
  if (typeof isSuccessful === "undefined") {
    isSuccessful = false;
  }

  return classNames({
    "text-negativecolor": !isSuccessful,
    "text-positivecolor": isSuccessful
  });
};

module.exports = {
  statusClassName: statusClassName
};
