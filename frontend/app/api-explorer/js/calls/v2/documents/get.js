(function (window) {

new APICallV2({
  category: ["main", "fetch"],
  name: "Get",
  description: "Get a document.",
  sampleUrl: "/api/v2/documents/$document_id$/get",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/get";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {
    'v1': 'Get'
  },
  params: [
          window.APIV2CallParamDocumentID
        ]
});

})(window);
