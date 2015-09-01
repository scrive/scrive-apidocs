(function (window) {

new APICallV2({
  category: "signing",
  name: "Prolong",
  description: "Prolong a document that has timed out.",
  sampleUrl: "/api/v2/documents/$document_id$/prolong",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/prolong";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Prolong'
  },
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "days",
            name: "Days",
            description: "Number of days to prolong document by.",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
