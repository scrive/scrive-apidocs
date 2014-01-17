/* This modal shows the user some feel good information with spinners and checkmarks.
 * Usage:
 * new SigningInProgressModalModel({
 *  document: document
 * });
 *
 * Only experted function is .done() that returns true if all spining magic is done;
 */


(function(window) {
  var SigningInProgressModalModel = Backbone.Model.extend({
    defaults : function() { // Making defaults a function will allow us to change of localization object
       return {
        actions : [localization.signinginprogressmodal.action1,localization.signinginprogressmodal.action2],
        currentActionIndex : 0,
        done: false,
        canFinish : false
        };
    },
    actions: function() {
      return this.get("actions");
    },
    getCurrentAction : function() {
      return this.actions()[this.get("currentAction")];
    },
    isCurrentAction : function(a) {
      return _.indexOf(this.actions(),a) == this.get("currentActionIndex");
    },
    isPassedAction : function(a) {
      return _.indexOf(this.actions(),a) < this.get("currentActionIndex");
    },
    startNextAction : function() {
      this.set({currentActionIndex : this.get("currentActionIndex") + 1});
    },
    lastAction : function() {
      return this.get("currentActionIndex") == this.actions().length - 1;
    },
    allActionsDone : function() {
      return this.get("currentActionIndex") >= this.actions().length;
    },
    setCanBeFinished : function() {
      return this.set({"canFinish": true});
    },
    canBeFinished : function() {
      return this.get("canFinish");
    },
    markAsDone: function() {
      return this.set({"done": true});
    },
    done: function() {
      return this.get("done");
    },
    document : function() {
      return this.get("document");
    },
    startActions : function(){
      var self = this;
      var f = function() {
        console.log("Changing action");
        if (!self.lastAction() || self.canBeFinished())
          self.startNextAction();
        if (!self.allActionsDone())
          setTimeout(f,2500);
        else
          setTimeout(function() {self.markAsDone();} ,500);
      };
      setTimeout(f,2500);
    }
  });

  var SigningInProgressModalView = Backbone.View.extend({
    initialize: function(args) {
      var self = this;
      _.bindAll(this, 'render','updateActions');
      this.model.bind('change', this.updateActions);
      this.render();
    },
    spinnerSmall: function() {
      return $(new Spinner({
        lines  : 9,
        length : 2,
        width  : 2,
        radius : 3,
        color  : '#000000',
        speed  : 1.5,
        trail  : 74,
        shadow : false,
        className: 'spinner'
      }).spin().el);
    },
    spinnerSmallSmallScreen: function() {
      return $(new Spinner({
        lines  : 10,
        length : 9,
        width  : 5,
        radius : 14,
        color  : '#000000',
        trail  : 74,
        shadow : false,
        className: 'spinner'
      }).spin().el);
    },
    updateActions : function() {
      var self = this;
      var model = this.model;
      self.spinnerCheckboxList.empty();
      self.spinnerCheckboxList.append("<li class='checkmark' style='display:none'/>"); // We append invisible position - so checkbox is loaded early
      _.each(model.actions(),function(a){
        var spinner = BrowserInfo.isSmallScreen() ? self.spinnerSmallSmallScreen() : self.spinnerSmall();
        var li = $("<li/>").text(a);
        if (model.isPassedAction(a)) li.addClass("checkmark");
        if (!model.isCurrentAction(a)) spinner.addClass("hidden");
        self.spinnerCheckboxList.append(li.append(spinner));
      });
    },
    render: function() {
      var self = this;
      var model = this.model;
      var container = $(this.el);
      var imgUrl = "/pages/" + model.document().mainfile().fileid() + "/1" + model.document().mainfile().queryPart();
      var docImage = $('<img class="img-doc" >').attr("alt",model.document().title()).attr("src",imgUrl);
      var title = $('<p class="title">'+localization.signinginprogressmodal.youHaveSigned+' <strong/>. '+localization.signinginprogressmodal.pleaseWaitWhile+'</p>');
      $("strong",title).text(model.document().title());

      this.spinnerCheckboxList = $('<ul class="spinner-checkbox-list">');
      var leftContainer = $('<div class="left-container">');
      var rightContainer = $('<div class="right-container">');

      // IE7 needs the spinner to be added to each element, it does not like the spinner element to be moved...
      self.updateActions();

      leftContainer.append(docImage);
      rightContainer.append(title);
      rightContainer.append(self.spinnerCheckboxList);
      container.append(leftContainer);
      container.append(rightContainer);
      container.append($('<div class="clearfix">'));

      return this;
    }
  });

  window.SigningInProgressModal = function(args){
    var model = new SigningInProgressModalModel(args);
    var view = new SigningInProgressModalView ({
        model: model,
        el: $("<div/>")
    });

    var confirmation = new Confirmation({
      title: localization.signinginprogressmodal.title,
      cssClass: 'grey post-sign-modal-content',
      fast : true,
      cancelVisible: false,
      closeVisible : false,
      acceptVisible: false,
      textcolor : args.textcolor,
      textfont : args.textfont,
      content: $(view.el),
      width: BrowserInfo.isSmallScreen() ? 825 : 520
    });
    model.startActions();
    return {
      close: function() {
        confirmation.clear();
      },
      setCanBeFinished : function() {
        model.setCanBeFinished();
      },
      done : function() {
        return model.done();
      }
    };
  };

})(window);
