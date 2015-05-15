(function (window) {

new APICallV2({
  category: "fetch",
  name: "Evidence Attachments",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/evidenceattachments",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/evidenceattachments";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "document_id",
            name: "$document_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          })
        ]
});

})(window);
