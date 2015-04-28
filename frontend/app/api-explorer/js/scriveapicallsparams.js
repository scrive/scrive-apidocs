
(function (window) {

window.APICallParam = Backbone.Model.extend({
  defaults: {
    type: "text",
    sendAsParam: true,
    useLocalStorage: false
  },
  type: function () {
          return this.get("type");
        },
  argName: function (i) {
          if (this.get("argName") instanceof Function) {
            return this.get("argName")(i);
          } else {
            return this.get("argName");
          }
        },
  name: function () {
          return this.get("name");
        },
  description: function () {
          return this.get("description");
        },
  sendAsParam: function () {
          return this.get("sendAsParam");
        },
  useLocalStorage: function () {
          return this.get("useLocalStorage");
        },
  limit: function () {
          return this.get("limit"); // Limit of files that can be uploaded
        },
  defaultValue: function () {
          if (this.useLocalStorage() && LocalStorage.get("param", this.argName())) {
            return LocalStorage.get("param", this.argName());
          } else if (this.get("defaultValue") instanceof Function) {
            return this.get("defaultValue")(this);
          } else {
            return this.get("defaultValue");
          }
        }
});

})(window);
