(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory- Send SMS PIN",
  description: "TODO",
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
            argName: "phone",
            name: "Phone number",
            description: "TODO",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
