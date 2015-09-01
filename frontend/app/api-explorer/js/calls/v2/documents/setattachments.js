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
            type: "file",
            argName: function (i) { return "attachment_" + i;},
            name: "attachments (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "The PDFs to attach to the document.\
                          These and ‘file_ids’ will be used to replace any existing attachments.",
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
          new APICallParam({
            type: "json",
            argName: "file_ids",
            name: "File Ids",
            sendAsParam: true,
            optional: true,
            description: "The existing attachment ‘file_ids’ to attach to the document.\
                          These and the ‘attachment_x’ will be used to replace any existing attachments.",
            defaultValue: "[]"
          })
          , window.APIV2CallParamObjectVersion
        ]
});

})(window);
