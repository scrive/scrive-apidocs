
(function (window) {

new APICallV1({
  category: ["main", "fetch"],
  name: "Get",
  description: "Fetches a document based on id",
  sampleUrl: "/api/v1/get/$documentid$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/get/" + this.get("documentid");
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of document that you want to fetch",
            defaultValue: ""
          })
        ]
});

})(window);
