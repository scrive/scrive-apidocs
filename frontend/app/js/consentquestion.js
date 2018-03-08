var Backbone = require("backbone");
var _ = require("underscore");

var ConsentQuestion = exports.ConsentQuestion = Backbone.Model.extend({
  defaults: {
    id: 0,
    title: "",
    positiveOption: "",
    negativeOption: "",
    response: null,
    detailedDescription: null,
  },

  initialize: function(args) {
    this.set({
      id: args.id,
      title: args.title,
      positiveOption: args.positive_option,
      negativeOption: args.negative_option,
      response: args.response,
      detailedDescription: args.detailed_description
    });
    return this;
  },

  draftData: function() {
    return {
      id: this.questionid(),
      title: this.title(),
      positive_option: this.positiveOption(),
      negative_option: this.negativeOption(),
      response: this.response(),
      detailed_description: this.get("detailedDescription")
    };
  },

  questionid: function() {
    return this.get("id");
  },

  title: function() {
    return this.get("title");
  },

  positiveOption: function() {
    return this.get("positiveOption");
  },

  negativeOption: function() {
    return this.get("negativeOption");
  },

  response: function() {
    return this.get("response");
  },

  hasResponse: function() {
    return this.response() != null;
  },

  isResponsePositive: function() {
    return this.response() == true;
  },

  isResponseNegative: function() {
    return this.response() == false;
  },

  setResponse: function(response) {
    this.set({ response: response });
  },

  hasDetailedDescription: function() {
    return this.get("detailedDescription") != null;
  },

  detailedDescriptionTitle: function() {
    return this.get("detailedDescription").title;
  },

  detailedDescriptionText: function() {
    return this.get("detailedDescription").text;
  }
});
