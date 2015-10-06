
(function (window) {

new APICallV1({
  category: "fetch",
  name: "Download file",
  description: "Downloads file that is connected to document.",
  sampleUrl: "/api/v1/downloadfile/$docid$/$fileid$/file.pdf",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/downloadfile/" + this.get("documentid")  + "/" + this.get("fileid") + "/file.pdf" ;
        },
  needsAuthorization: true,
  expectBinaryResponse: true,
  equivalentCalls: {
    "v2": "Files: Get File"
  },
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of document.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "fileid",
            name: "File id",
            sendAsParam: false,
            description: "Id of file.",
            defaultValue: ""
          })
        ]
});

})(window);
