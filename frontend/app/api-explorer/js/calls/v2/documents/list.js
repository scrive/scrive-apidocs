(function (window) {

new APICallV2({
  category: "fetch",
  name: "List",
  description: "Fetch a list of documents, with sorting and filtering options.",
  sampleUrl: "/api/v2/documents/list",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/list";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'List'
  },
  params: [
          new APICallParam({
            type: "text",
            argName: "offset",
            name: "Offset",
            sendAsParam: true,
            description: "Starting offset for documents to return. If offset is larger than the total number of matching documents an empty list is returned.",
            defaultValue: "0"
          }),
          new APICallParam({
            type: "text",
            argName: "max",
            optional: true,
            name: "Max",
            sendAsParam: true,
            description: "Maximum number of documents to return. Server may cap to a lower value.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "sorting",
            optional: true,
            name: "Sorting",
            sendAsParam: true,
            description: "Filtering options.",
            defaultValue: "[]"
          }),
          new APICallParam({
            type: "json",
            argName: "filter",
            optional: true,
            name: "Filter",
            sendAsParam: true,
            description: "Sorting options.",
            defaultValue: "[]"
          })
        ]
});

})(window);
