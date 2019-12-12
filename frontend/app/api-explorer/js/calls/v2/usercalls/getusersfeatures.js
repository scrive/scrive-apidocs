/*
 * API demo main model + view
 */

(function (window) {

new APICallV2({
  category: "v2-user-related",
  name: "Get user features",
  description: "Gets features of user that is currently authorised.",
  sampleUrl: "/api/v2/getusersfeatures",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/getusersfeatures";
        },
  needsAuthorization: true,
  params: []
});

})(window);
