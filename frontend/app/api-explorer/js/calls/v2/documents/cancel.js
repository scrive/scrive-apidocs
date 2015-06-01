(function (window) {

new APICallV2({
  category: "signing",
  name: "Cancel",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/cancel",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/cancel";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Cancel'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
