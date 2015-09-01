(function (window) {

new APICallV2({
  category: "other",
  name: "Clone",
  description: "Clone a document. The new document will have status as ‘preparation’ and will use current author data as the first signatory.",
  sampleUrl: "/api/v2/documents/$document_id$/clone",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/clone";
        },
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
