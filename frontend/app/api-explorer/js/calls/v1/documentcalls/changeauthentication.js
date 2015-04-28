
(function (window) {

new APICallV1({
  category: "signing",
  name: "Change authentication",
  description: "Change authentication for any signatory of document",
  sampleUrl: "/api/v1/changeauthentication/$docid$/$signatoryid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/changeauthentication/" + this.get("documentid") + "/" + this.get("signatoryid");
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of document",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "signatoryid",
            name: "Signatory id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of signatory",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "authentication_type",
            name: "Type of authentication",
            sendAsParam: true,
            description: "Type of authentication that will be used. Possible values: standard, sms_pin and eleg",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "authentication_value",
            name: "Value for authentication",
            sendAsParam: true,
            description: "Optional. Depending on type of authentication it can be phone number or SSN",
            defaultValue: ""
          })
        ]
});

})(window);
