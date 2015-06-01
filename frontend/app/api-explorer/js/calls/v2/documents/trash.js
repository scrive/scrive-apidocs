(function (window) {

new APICallV2({
  category: "signing",
  name: "Trash",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/trash",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/trash";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Delete'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
