(function (window) {

new APICallV2({
  category: "signing",
  name: "Restart",
  description: "Restart a document that has been cancelled, timed out, or rejected.",
  sampleUrl: "/api/v2/documents/$document_id$/restart",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/restart";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Restart'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
