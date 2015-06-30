(function (window) {

new APICallV2({
  category: "fetch",
  name: "Files: Get Main",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/files/main/$file_name$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/files/main/" + this.get("file_name");
        },
  needsAuthorization: true,
  expectBinaryResponse: true,
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
            argName: "file_name",
            name: "$file_name$",
            sendAsParam: false,
            description: "TODO",
            defaultValue: ""
          })
        ]
});

})(window);
