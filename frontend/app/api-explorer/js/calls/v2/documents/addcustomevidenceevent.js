(function (window) {

new APICallV2({
  category: "other",
  name: "Add evidence event",
  description: "Add an event with custom text to a document's evidence log",
  sampleUrl: "/api/v2/documents/$document_id$/addevidenceevent",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/addevidenceevent";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {},
  params: [
          window.APIV2CallParamDocumentID
        , new APICallParam({
            type: "json",
            argName: "Event",
            name: "event",
            sendAsParam: true,
            optional: false,
            description: "The custom text to set in the event"
          })
  ]
});

})(window);
