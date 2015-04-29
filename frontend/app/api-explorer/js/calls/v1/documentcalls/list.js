
(function (window) {

new APICallV1({
  category: "fetch",
  name: "List",
  description: "Fetches list of documents",
  sampleUrl: "/api/v1/list",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/list";
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentType",
            name: "Document type",
            sendAsParam: true,
            description: "Type of documents you want to list. Possible options: All, Document, Template. " +
                         "If not set: will list all documents that are not deleted",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "offset",
            name: "Offset",
            sendAsParam: true,
            description: "",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "limit",
            name: "Limit",
            sendAsParam: true,
            description: "",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "tags",
            name: "Tags",
            sendAsParam: true,
            description: "List of tags that should be used for filtering.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "selectfilters",
            name: "Select filters",
            sendAsParam: true,
            description: "List of select filters as described in documentation.",
            defaultValue: ""
          })
        ]
});

})(window);
