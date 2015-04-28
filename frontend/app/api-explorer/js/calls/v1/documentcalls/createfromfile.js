
(function (window) {

new APICallV1({
  category: ["draft", "main"],
  name: "Create from file",
  description: "Creates document based on file",
  sampleUrl: "/api/v1/createfromfile",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/createfromfile/";
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          new APICallParam({
            type: "file",
            argName: function () { return "file";},
            name: "File",
            sendAsParam: true,
            description: "File that will be main file for new document",
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
        ]
});

})(window);
