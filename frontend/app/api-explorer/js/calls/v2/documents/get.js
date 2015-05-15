(function (window) {

new APICallV2({
  category: "fetch",
  name: "Get",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/get",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/get";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "document_id",
            name: "$document_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          })
        ]
});

})(window);
