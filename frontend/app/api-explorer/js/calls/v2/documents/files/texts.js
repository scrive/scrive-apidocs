(function (window) {

new APICallV2({
  category: "fetch",
  name: "Document Texts",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/texts/$file_id$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/texts/" + this.get("file_id");
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
          }),
          new APICallParam({
            type: "text",
            argName: "file_id",
            name: "$file_id$",
            sendAsParam: false,
            description: "TODO",
            defaultValue: ""
          })
        ]
});

})(window);
