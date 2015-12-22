(function (window) {

new APICallV2({
  category: "signing",
  name: "Callback",
  description: "Trigger a API callback to the URL set for the document (if one is set, no effect otherwise).",
  sampleUrl: "/api/v2/documents/$document_id$/callback",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/callback";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Trigger callback'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
