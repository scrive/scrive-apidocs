(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "New",
  description: "Create a new document, optionally with the given PDF as the main file.",
  sampleUrl: "/api/v2/documents/new",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/new";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {
    'v1': 'Create from file'
  },
  params: [
          new APICallParam({
            type: "file",
            argName: function () { return "file";},
            name: "file (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "The PDF to use in the new document.",
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
        , new APICallParam({
            type: "bool",
            argName: "saved",
            name: "saved",
            sendAsParam: true,
            optional: true,
            description: "Whether the document should start out as being saved\
                          and thus appear in the E-archive.",
            defaultValue: true
          })
        , new APICallParam({
            type: "text",
            argName: "folder_id",
            sendAsParam: true,
            optional: true,
            description: "ID of the folder to save the document to"
        })
        ]
});

})(window);
