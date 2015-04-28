
(function (window) {

new APICallV1({
  category: "fetch",
  name: "Download main file",
  description: "Downloads file of document document",
  sampleUrl: "/api/v1/downloadmainfile/$docid$/file.pdf",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/downloadmainfile/" + this.get("documentid")  + "/file.pdf" ;
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
          })
        ]
});

})(window);
