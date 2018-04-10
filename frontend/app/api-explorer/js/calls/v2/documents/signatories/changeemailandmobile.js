(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory - Change Email and Mobile",
  description: "Change the email address and mobile number of a signatory after the document has been started.",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/changeemailandmobile",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/changeemailandmobile";
        },
  needsAuthorization: true,
  isInternal: true,
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          new APICallParam({
            type: "text",
            argName: "personal_number",
            name: "Personal Number",
            optional: true,
            optionToSendEmpty: true,
            description: "The new email address for the signatory. Whilst this field is optional, both email and mobile_number cannot be blank, you need at least one.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "mobile_number",
            name: "Mobile Number",
            optional: true,
            optionToSendEmpty: true,
            description: "The new mobile number for the signatory. Whilst this field is optional, both email and mobile_number cannot be blank, you need at least one.",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
