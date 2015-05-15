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
  params: [
          new APICallParam({
            type: "text",
            argName: "document_id",
            name: "$document_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          }),
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
            description: "TODO",
          }),
          window.APICallParamObjectVersion
        ]
});

})(window);
