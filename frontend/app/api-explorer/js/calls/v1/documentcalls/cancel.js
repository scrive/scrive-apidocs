
(function (window) {

new APICallV1({
  category: "signing",
  name: "Cancel",
  description: "Cancel document.",
  sampleUrl: "/api/v1/cancel/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/cancel/" + this.get("documentid");
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
            description: "Optional. Version of document to be updated. If it will not match " +
                         "current document version and call would change document, whole call will be rejected. ",
            defaultValue: ""
          })
        ]
});

})(window);
