
(function (window) {

new APICallV1({
  category: "fetch",
  name: "History",
  description: "Fetches history of a document",
  sampleUrl: "/api/v1/history/$documentid$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/history/" + this.get("documentid");
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
            defaultValue: "1"
          })
        ]
});

})(window);
