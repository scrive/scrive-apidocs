(function (window) {

new APICallV2({
  category: "fetch",
  name: "History",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/history",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/history";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {
    'v1': 'History'
  },
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "lang",
            optional: true,
            name: "Language",
            sendAsParam: true,
            description: "Language to get the history events in.",
            defaultValue: ""
          })
        ]
});

})(window);
