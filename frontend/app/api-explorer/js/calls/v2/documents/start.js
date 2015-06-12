(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "Start",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/start",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/start";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Ready'
  },
  params: [
          window.APIV2CallParamDocumentID,
        , new APICallParam({
            type: "bool",
            argName: "author_signs_now",
            name: "author_signs_now",
            sendAsParam: true,
            optional: true,
            description: "Whether the document author signs immediately as part\
                          of the start call.",
            defaultValue: false
          })
        , window.APIV2CallParamObjectVersion
        ]
});

})(window);
