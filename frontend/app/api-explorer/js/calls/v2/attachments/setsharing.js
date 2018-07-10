(function (window) {

new APICallV2({
  category: "attachment",
  name: "Share attachments",
  description: "Share or unshare attachments.",
  sampleUrl: "/api/v2/attachments/setsharing",
  method: "POST",
  getCallUrl: function () {
    return "/api/v2/attachments/setsharing";
  },
  needsAuthorization: true,
  params: [
    new APICallParam({
      type: "json",
      argName: "attachment_ids",
      optional: false,
      name: "Attachment IDs",
      sendAsParam: true,
      description: "IDs of the attachments to share or unshare.",
      defaultValue: "[]"
    }),
    new APICallParam({
      type: "bool",
      argName: "shared",
      optional: true,
      name: "Shared",
      sendAsParam: true,
      description: "Whether the attachments should be shared or not.",
      defaultValue: ""
    })
  ]
});

})(window);
