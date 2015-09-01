(function (window) {

new APICallV2({
  category: "draft",
  name: "Document Texts",
  description: "Extract text from the main file of a document.",
  sampleUrl: "/api/v2/documents/$document_id$/texts/$file_id$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/texts/" + this.get("file_id");
        },
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "file_id",
            name: "$file_id$",
            sendAsParam: false,
            description: "The file ID of the document's main file",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "json",
            name: "json",
            sendAsParam: true,
            description: "A JSON representation of rectangles to extract text from.",
            defaultValue: "{}"
          })
        ]
});

})(window);
