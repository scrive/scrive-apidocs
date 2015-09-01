(function (window) {

new APICallV2({
  category: "draft",
  name: "Set File",
  description: "Set the file for a document in preparation.",
  sampleUrl: "/api/v2/documents/$document_id$/setfile",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/setfile";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Change main file'
  },
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "file",
            argName: function () { return "file";},
            name: "file (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "If provided, the PDF will be set as the file for the document.\
                          If not provided the current file for the document will be removed.",
            limit: 1,
            defaultValue: function (self) {
              var input = $("<input type='file' class='form-control multiFileInput'/>");
              input.MultiFile({
                afterFileAppend: function (input, title, fileinput) {
                  setTimeout(function () {self.trigger("files-changed");}, 10);
                }
              });
              return input.data("MultiFile");
            }
          })
          , window.APIV2CallParamObjectVersion
        ]
});

})(window);
