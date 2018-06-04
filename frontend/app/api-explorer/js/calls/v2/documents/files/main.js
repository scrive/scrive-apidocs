(function (window) {

new APICallV2({
  category: "fetch",
  name: "Files: Get Main",
  description: "Get the main file of a document.",
  sampleUrl: "/api/v2/documents/$document_id$/files/main/$filename$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/files/main/" + this.get("filename");
        },
  needsAuthorization: true,
  expectBinaryResponse: true,
  expectPDFResponse: true,
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "filename",
            name: "$filename$",
            sendAsParam: false,
            description: "Arbitrary filename for downloading",
            defaultValue: ""
          })
        ]
});

})(window);
