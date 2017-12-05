(function (window) {

new APICallV2({
  category: "fetch",
  name: "Signatory: Get QR Code",
  description: "Get the signatory's sign link as a QR Code",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/getqrcode",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/getqrcode";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  expectBinaryResponse: true,
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
