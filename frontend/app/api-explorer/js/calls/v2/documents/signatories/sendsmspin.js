(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory- Send SMS PIN",
  description: "Send an SMS PIN for the signatory to authenticate.",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/sendsmspin",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/sendsmspin";
        },
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          new APICallParam({
            type: "text",
            argName: "mobile",
            name: "Phone number",
            description: "If the phone number is not set by document author,\
                          then it must be provided.",
            optional: true,
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
