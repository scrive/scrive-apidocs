(function (window) {

new APICallV1({
  category: "signing",
  name: "Change authentication to view",
  description: "Change authentication to view for any signatory of document.",
  sampleUrl: "/api/v1/changeauthenticationtoview/$docid$/$signatoryid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/changeauthenticationtoview/" + this.get("documentid") + "/" + this.get("signatoryid");
        },
  needsAuthorization: true,
  equivalentCalls: {
    "v2": "Signatory- Set Authentication to View"
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
            argName: "signatoryid",
            name: "Signatory id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of signatory.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "authentication_type",
            name: "Type of authentication",
            sendAsParam: true,
            description: "Type of authentication that will be used. Possible values: standard, se_bankid, no_bankid.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "personal_number",
            name: "Personal number",
            sendAsParam: true,
            optional: true,
            optionToSendEmpty: true,
            description: "Personal number to set for the signatory, if the authentication method requires it.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "mobile_number",
            name: "Mobile number",
            sendAsParam: true,
            optional: true,
            description: "Phone number to use for Norwegian BankID",
            defaultValue: ""
          })
        ]
});

})(window);
