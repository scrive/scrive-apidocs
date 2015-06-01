(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory- Set Authentication",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/setauthentication",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/setauthentication";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Change authentication'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          new APICallParam({
            type: "text",
            argName: "authentication_type",
            name: "Authentication Type",
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "authentication_value",
            name: "Authentication Value",
            optional: true,
            optionToSendEmpty: true,
            description: "TODO",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
