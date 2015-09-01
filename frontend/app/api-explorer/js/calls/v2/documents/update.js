(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "Update",
  description: "Update a document in preparation.",
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
            description: "The updated Document JSON.\
                          This can be a partial Document JSON representing only\
                          the parts that need updating.\
                          The call will always return the full Document JSON.",
            defaultValue: "{}"
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
