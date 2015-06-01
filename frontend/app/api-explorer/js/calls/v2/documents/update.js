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
  equivalentCalls: {
    'v1': 'Update'
  },
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "json",
            argName: "document",
            name: "Document (JSON)",
            sendAsParam: true,
            description: "TODO",
            defaultValue: "{}"
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
