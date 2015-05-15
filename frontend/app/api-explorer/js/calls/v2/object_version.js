(function (window) {
window.APICallParamObjectVersion = new APICallParam({
  type: "text",
  argName: "object_version",
  name: "Object version",
  sendAsParam: true,
  optional: true,
  description: "If provided: the server will check that it matches the current \
               object version and will only process the request if it matches, \
               otherwise you will get an error for object version mismatch.",
  defaultValue: ""
})
})(window);
