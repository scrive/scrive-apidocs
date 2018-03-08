var Backbone = require("backbone");
var _ = require("underscore");
var ConsentQuestion = require("./consentquestion").ConsentQuestion;

var ConsentModule = exports.ConsentModule = Backbone.Model.extend({
  defaults: {
    title: "",
    questions: []
  },

  initialize: function(args) {
    this.set({
      title: args.title,
      questions: _.map(args.questions, function(question) {
        return new ConsentQuestion(question);
      })
    });

    return this;
  },

  title: function() {
    return this.get("title");
  },

  questions: function() {
    return this.get("questions");
  },

  draftData: function() {
    return {
      title: this.title(),
      questions: _.map(this.questions(), function (question) {
        return question.draftData();
      })
    };
  }
});
