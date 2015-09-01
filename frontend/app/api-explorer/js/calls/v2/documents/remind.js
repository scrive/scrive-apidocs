(function (window) {

new APICallV2({
  category: "signing",
  name: "Remind",
  description: "Send a reminder invitation message to all participants waiting to sign.",
  sampleUrl: "/api/v2/documents/$document_id$/remind",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/remind";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Send reminder'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
