
(function (window) {

new APICallV1({
  category: "fetch",
  name: "Download main file",
  description: "Downloads the main file of the document.",
  sampleUrl: "/api/v1/downloadmainfile/$docid$/file.pdf",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/downloadmainfile/" + this.get("documentid")  + "/file.pdf" ;
        },
  needsAuthorization: true,
  expectBinaryResponse: true,
  equivalentCalls: {
    'v2': 'Files: Get Main'
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
          })
        ]
});

})(window);
