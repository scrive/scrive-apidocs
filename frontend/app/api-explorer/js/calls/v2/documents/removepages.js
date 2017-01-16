(function (window) {

new APICallV2({
  category: "draft",
  name: "Remove pages",
  description: "Remove pages from main file of draft or template.",
  sampleUrl: "/api/v2/documents/$document_id$/removepages",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/removepages";
        },
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "pages",
            name: "Pages",
            description: "List of pages to be removed. In [n1,n2,...,nN] format",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
