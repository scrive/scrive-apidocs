(function (window) {

new APICallV2({
  category: "draft",
  name: "Set Attachments",
  description: "TODO",
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
            description: "TODO",
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
            description: "TODO",
            defaultValue: "[]"
          })
          , window.APIV2CallParamObjectVersion
        ]
});

})(window);
