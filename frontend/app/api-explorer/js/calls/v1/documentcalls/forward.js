
(function (window) {

new APICallV1({
  category: "signing",
  name: "Forward",
  description: "Forward confirmation mail for any signed document to given email address",
  sampleUrl: "/api/v1/forward/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/forward/" + this.get("documentid");
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
            argName: "email",
            name: "Email",
            description: "",
            defaultValue: ""
          }),
          new APICallParam({
            type: "bool",
            argName: "nocontent",
            name: "No content",
            description: "If set, email will contain attachments, but rest of content will be empty.",
            defaultValue: ""
          })
        ]
});

})(window);
