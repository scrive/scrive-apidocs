(function (window) {

new APICallV2({
  category: "other",
  name: "Add image",
  description: "Add an image to main pdf file",
  sampleUrl: "/api/v2/documents/$document_id$/addimage",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/addimage";
        },
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "pageno",
            optional: false,
            name: "Page",
            sendAsParam: true,
            description: "Page",
            defaultValue: "1"
          }),
          new APICallParam({
            type: "text",
            argName: "x",
            optional: false,
            name: "x",
            sendAsParam: true,
            description: "x",
            defaultValue: "0.5"
          }),
          new APICallParam({
            type: "text",
            argName: "y",
            optional: false,
            name: "y",
            sendAsParam: true,
            description: "y",
            defaultValue: "0.5"
          }),
          new APICallParam({
            type: "text",
            argName: "image",
            optional: false,
            name: "Image",
            sendAsParam: true,
            description: "Image RFC2397 encoded",
            defaultValue: ""
          })
        ]
});

})(window);
