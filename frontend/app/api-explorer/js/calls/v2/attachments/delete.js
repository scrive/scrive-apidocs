(function (window) {

new APICallV2({
  category: "attachment",
  name: "Delete attachments",
  description: "Delete some attachments.",
  sampleUrl: "/api/v2/attachments/delete",
  method: "POST",
  getCallUrl: function () {
    return "/api/v2/attachments/delete";
  },
  needsAuthorization: true,
  params: [
    new APICallParam({
      type: "json",
      argName: "attachment_ids",
      optional: false,
      name: "Attachment IDs",
      sendAsParam: true,
      description: "IDs of the attachments to delete.",
      defaultValue: "[]"
    })
  ]
});

})(window);
