
(function (window) {

new APICallV1({
  category: "signing",
  name: "Trigger callback",
  description: "Trigger a callback for a document.",
  sampleUrl: "/api/v1/triggercallback/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/triggercallback/" + this.get("documentid");
        },
  needsAuthorization: true,
  equivalentCalls: {
    "v2": "Callback"
  },
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of document to trigger the callback for.",
            defaultValue: ""
          })
        ]
});

})(window);
