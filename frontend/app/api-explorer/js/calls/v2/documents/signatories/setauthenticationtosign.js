(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory - Set Authentication to Sign",
  description: "Set the signatory authentication to sign method after the\
                document has been started.\
                Side-effects of this operation may include adding or modifying\
                fields for the signatory.",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/setauthenticationtosign",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/setauthenticationtosign";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Change authentication to sign'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          new APICallParam({
            type: "text",
            argName: "authentication_type",
            name: "Authentication Type",
            description: "The authentication method to set for the signatory.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "personal_number",
            name: "Personal Number",
            optional: true,
            optionToSendEmpty: true,
            description: "If the authentication_type requires a personal number, and the signatory doesn’t have one set already, then it must be provided and valid for the chosen authentication-to-view method.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "mobile_number",
            name: "Mobile Number",
            optional: true,
            optionToSendEmpty: true,
            description: "Can be used for authentication_type that makes use of a mobile number. Similar requirements as personal_number.",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
