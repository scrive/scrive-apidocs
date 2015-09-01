(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "Start",
  description: "Start the signing process for a document in preparation.",
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
            description: "Setting to ‘true’ will result in no invitation to\
                          sign being sent to the author.\
                          This is useful for when the author signs immediately\
                          after the ‘start’ call.",
            defaultValue: false
          })
        , window.APIV2CallParamObjectVersion
        ]
});

})(window);
