(function (window) {

new APICallV2({
  category: "fetch",
  name: "History",
  description: "Get the document history to display to the user.",
  sampleUrl: "/api/v2/documents/$document_id$/history",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/history";
        },
  needsAuthorization: true,
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
            description: "The language to display the history in. Has to be a supported and valid two letter language code. Defaults to user setting.",
            defaultValue: ""
          })
        ]
});

})(window);
