(function (window) {

new APICallV2({
  category: "attachment",
  name: "Download attachment",
  description: "Get an attachment's PDF file.",
  sampleUrl: "/api/v2/attachments/$attachment_id$/download/$filename$",
  method: "GET",
  getCallUrl: function () {
    return "/api/v2/attachments/" + this.get("attachment_id") + "/download/"
                                  + this.get("filename");
  },
  needsAuthorization: true,
  expectBinaryResponse: true,
  params: [
    new APICallParam({
      type: "text",
      argName: "attachment_id",
      name: "$attachment_id$",
      sendAsParam: false,
      description: "The attachment ID",
      defaultValue: ""
    }),
    new APICallParam({
      type: "text",
      argName: "filename",
      name: "$filename$",
      sendAsParam: false,
      description: "Arbitrary filename for downloading",
      defaultValue: ""
    })
  ]
});

})(window);
