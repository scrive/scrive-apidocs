
(function (window) {

new APICallV1({
  category: "signing",
  name: "Restart",
  description: "Restart document. A new document will be created",
  sampleUrl: "/api/v1/restart/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/restart/" + this.get("documentid");
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
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
