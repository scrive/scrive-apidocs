
(function (window) {

new APICallV1({
  category: "signing",
  name: "Send reminder",
  description: "Send reminder to all signatories that have not yet signed the document",
  sampleUrl: "/api/v1/remind/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/remind/" + this.get("documentid");
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
          })
        ]
});

})(window);
