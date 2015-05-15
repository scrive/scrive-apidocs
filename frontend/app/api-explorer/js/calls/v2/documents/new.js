(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "New",
  description: "Create a new document, optionally with the given PDF as the main file",
  sampleUrl: "/api/v2/documents/new",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/new";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          new APICallParam({
            type: "file",
            argName: function () { return "file";},
            name: "file (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "PDF to be used as main file for new document.",
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
          , window.APICallParamObjectVersion
        ]
});

})(window);
