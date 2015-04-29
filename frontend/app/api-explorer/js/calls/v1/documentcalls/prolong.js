
(function (window) {

new APICallV1({
  category: "signing",
  name: "Prolong",
  description: "Prolong document that has timed out",
  sampleUrl: "/api/v1/prolong/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/prolong/" + this.get("documentid");
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
            argName: "days",
            name: "Days",
            description: "",
            defaultValue: "1"
          })
        ]
});

})(window);
