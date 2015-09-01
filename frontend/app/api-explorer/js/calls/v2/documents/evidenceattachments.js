(function (window) {

new APICallV2({
  category: "fetch",
  name: "Evidence Attachments",
  description: "Returns the list of evidence attachments for a document. List can be empty.",
  sampleUrl: "/api/v2/documents/$document_id$/evidenceattachments",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/evidenceattachments";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          window.APIV2CallParamDocumentID
        ]
});

})(window);
