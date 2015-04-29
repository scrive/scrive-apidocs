
(function (window) {

new APICallV1({
  category: ["draft", "main"],
  name: "Update",
  description: "Update API call, updates the document based on the given JSON",
  sampleUrl: "/api/v1/update/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/update/" + this.get("documentid");
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
            description: "Optional. Version of document to be updated. If it will does not match the current " +
                         "document version and call will change the document, the entire call will be rejected. ",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "json",
            name: "Document JSON",
            sendAsParam: true,
            description: "Document structure that will be used to update existing document",
            defaultValue: "{}"
          })
        ]
});

})(window);
