(function (window) {

new APICallV2({
  category: "draft",
  name: "Set Attachments",
  description: "Set or remove attachments for the document. Replaces any existing attachments.",
  sampleUrl: "/api/v2/documents/$document_id$/setattachments",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/setattachments";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Set author attachments'
  },
  params: [
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "json",
            argName: "attachments",
            optional: false,
            name: "Attachments",
            sendAsParam: true,
            description: "List of attachments. Replaces any existing attachments.",
            defaultValue: "[]"
          }),
          new APICallParam({
            type: "file",
            argName: function (i) { return "attachment_file_" + i;},
            name: "Attachment Files (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "The PDFs to attach to the document.\
                          The parameter names can be used in the attachments\
                          JSON as ‘file’ values.\
                          See API documentation for more details.",
            limit: 100,
            defaultValue: function (self) {
              var input = $("<input type='file' class='form-control multiFileInput'/>");
              input.MultiFile({
                afterFileAppend: function (input, title, fileinput) {
                  setTimeout(function () {self.trigger("files-changed");}, 10);
                }
              });
              return input.data("MultiFile");
            }
          }),
          , window.APIV2CallParamObjectVersion
        ]
});

})(window);
