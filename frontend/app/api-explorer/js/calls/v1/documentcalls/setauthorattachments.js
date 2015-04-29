
(function (window) {

new APICallV1({
  category: "draft",
  name: "Set author attachments",
  description: "Sets author attachment for given document",
  sampleUrl: "/api/v1/setattachments/$documentid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/setattachments/" + this.get("documentid");
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            description: "",
            useLocalStorage: true,
            defaultValue: ""
          }),
          new APICallParam({
            type: "file",
            argName: function (i) { return "attachment_" + i;},
            name: "Attachments",
            sendAsParam: true,
            description: "Attachment files",
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
          })
        ]
});

})(window);
