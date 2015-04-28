
(function (window) {

new APICallV1({
  category: ["draft", "main"],
  name: "Ready",
  description: "Make document ready for signing",
  sampleUrl: "/api/v1/ready/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/ready/" + this.get("documentid");
        },
  needsAuthorization: true,
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
            argName: "objectversion",
            name: "Object version",
            sendAsParam: true,
            description: "Optional. Version of document to be updated. If it will not match current " +
                         "document version and call would change document, whole call will be rejected. ",
            defaultValue: ""
          })
        ]
});

})(window);
