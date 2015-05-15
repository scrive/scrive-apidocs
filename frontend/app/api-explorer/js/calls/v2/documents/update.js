(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "Update",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/update",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/update";
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "document_id",
            name: "$document_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "document",
            name: "Document (JSON)",
            sendAsParam: true,
            description: "TODO",
            defaultValue: "{}"
          }),
          window.APICallParamObjectVersion
        ]
});

})(window);
