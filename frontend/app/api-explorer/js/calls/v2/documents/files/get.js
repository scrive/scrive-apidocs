(function (window) {

new APICallV2({
  category: "fetch",
  name: "Files: Get File",
  description: "Get a file.",
  sampleUrl: "/api/v2/documents/$document_id$/files/$file_id$/$file_name$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/files/"
                                      + this.get("file_id") + "/"
                                      + this.get("file_name");
        },
  needsAuthorization: true,
  expectBinaryResponse: true,
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "file_id",
            name: "$file_id$",
            sendAsParam: false,
            description: "The file ID",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "file_name",
            name: "$file_name$",
            sendAsParam: false,
            description: "Arbitrary filename for downloading",
            defaultValue: ""
          })
        ]
});

})(window);
