module.exports = function (string) {
  if (string == undefined || string.length == 0) return string;
  return string.charAt(0).toUpperCase() + string.slice(1);
};
