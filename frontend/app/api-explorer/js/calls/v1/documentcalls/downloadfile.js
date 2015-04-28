
(function (window) {

new APICallV1({
  category: "fetch",
  name: "Download file",
  description: "Downloads file that is connected to document",
  sampleUrl: "/api/v1/downloadfile/$docid$/$fileid$/file.pdf",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/downloadfile/" + this.get("documentid")  + "/" + this.get("fileid") + "/file.pdf" ;
        },
  needsAuthorization: true,
  expectBinaryResponse: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "fileid",
            name: "File id",
            sendAsParam: false,
            description: "",
            defaultValue: ""
          })
        ]
});

})(window);
