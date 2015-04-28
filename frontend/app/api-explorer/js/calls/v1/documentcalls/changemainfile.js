
(function (window) {

new APICallV1({
  category: "draft",
  name: "Change main file",
  description: "Change main file of document.",
  sampleUrl: "/api/v1/changemainfile/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/changemainfile/" + this.get("documentid");
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "Id of document that you want to fetch",
            defaultValue: ""
          }),
          new APICallParam({
            type: "file",
            argName: function () { return "file";},
            name: "File",
            sendAsParam: true,
            description: "File that will be main file for document.",
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
