/*
 * API demo main model + view
 */

(function (window) {

new APICallV1({
  category: "other",
  name: "Get profile",
  description: "Gets profile of user that is currently authorised",
  sampleUrl: "/api/v1/getprofile",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/getprofile";
        },
  needsAuthorization: true,
  params: []
});

})(window);
