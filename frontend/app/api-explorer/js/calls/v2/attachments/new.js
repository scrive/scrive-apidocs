(function (window) {

new APICallV2({
  category: "attachment",
  name: "Create attachment",
  description: "Add a new attachment.",
  sampleUrl: "/api/v2/attachments/new",
  method: "POST",
  getCallUrl: function () {
    return "/api/v2/attachments/new";
  },
  needsAuthorization: true,
  params: [
    new APICallParam({
      type: "text",
      argName: "title",
      optional: true,
      name: "Title",
      sendAsParam: true,
      description: "Attachment title. If not present, it will be inferred from the file name",
      defaultValue: ""
    }),
    new APICallParam({
      type: "file",
      argName: function () { return "file";},
      name: "file (application/pdf)",
      sendAsParam: true,
      required: true,
      description: "The attachment's PDF file.",
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
