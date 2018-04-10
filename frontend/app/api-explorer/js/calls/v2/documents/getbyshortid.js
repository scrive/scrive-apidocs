(function (window) {

new APICallV2({
  category: ["fetch"],
  name: "Get by short ID",
  description: "Get a document using a short 6 digit Document ID. Only valid for documents created or modified in the last 24 hours that are Pending.",
  sampleUrl: "/api/v2/documents/$short_document_id$/getbyshortid",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("short_document_id") + "/getbyshortid";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {
    'v1': 'Get'
  },
  params: [
          new APICallParam({
            type: "text",
            argName: "short_document_id",
            name: "$short_document_id$",
            sendAsParam: false,
            useLocalStorage: false,
            description: "The last 6 digits of a Document ID",
            defaultValue: ""
          })
        ]
});

})(window);
