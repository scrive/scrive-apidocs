(function (window) {

new APICallV2({
  category: "signing",
  name: "Forward",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/forward",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/forward";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Forward'
  },
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "email",
            name: "Email",
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "bool",
            argName: "no_content",
            optional: true,
            name: "No content",
            description: "TODO"
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
