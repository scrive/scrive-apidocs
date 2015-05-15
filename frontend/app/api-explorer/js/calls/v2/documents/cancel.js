(function (window) {

new APICallV2({
  category: "signing",
  name: "Cancel",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/cancel",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/cancel";
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
          window.APICallParamObjectVersion
        ]
});

})(window);
