(function (window) {

window.APIV2CallParamDocumentID = new APICallParam({
  type: "text",
  argName: "document_id",
  name: "$document_id$",
  sendAsParam: false,
  useLocalStorage: true,
  description: "The document ID",
  defaultValue: ""
}),

window.APIV2CallParamSignatoryID = new APICallParam({
  type: "text",
  argName: "signatory_id",
  name: "$signatory_id$",
  sendAsParam: false,
  useLocalStorage: true,
  description: "The signatory ID",
  defaultValue: ""
}),

window.APIV2CallParamObjectVersion = new APICallParam({
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
