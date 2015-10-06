
(function (window) {

new APICallV1({
  category: "signing",
  name: "Prolong",
  description: "Prolong document that has timed out.",
  sampleUrl: "/api/v1/prolong/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/prolong/" + this.get("documentid");
        },
  needsAuthorization: true,
  equivalentCalls: {
    "v2": "Prolong"
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
            argName: "days",
            name: "Days",
            description: "Number of days to prolong the document's deadline by.",
            defaultValue: "1"
          })
        ]
});

})(window);
