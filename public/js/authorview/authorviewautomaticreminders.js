(function(window){

var AuthorViewAutomaticRemindersModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
    if (this.document().autoremindtime() != undefined)
      this.set({newdaystoremind: this.document().autoremindtime().diffDays()+1});
    else
      this.set({newdaystoremind: Math.max(1,Math.floor(this.maxdays() / 2))});
  },
  authorview : function() {
     return this.get("authorview");
  },
  maxdays : function() {
    return Math.max(1,this.document().timeouttime().diffDays());

  },
  newdaystoremind: function() {
     return this.get("newdaystoremind");
  },
  setNewdaystoremind: function(newdaystoremind) {
     var old = this.get("newdaystoremind")
     if (newdaystoremind == undefined || (1 <= newdaystoremind || newdaystoremind <= this.maxdays())) {
      this.set({"newdaystoremind": newdaystoremind}, {silent: true});
     }
     if (old != this.get("newdaystoremind"))
        this.trigger("change:newdaystoremind");
  },
  document :function() {
     return this.authorview().document();
  },
  setautoreminder : function(days, callback) {
     var self = this;
     this.document().setautoreminder(days).sendAjax(function() {
          if (callback!= undefined) callback();
          self.authorview().reload(true);

    });
  }
});

var AuthorViewAutomaticRemindersView = Backbone.View.extend({
  initialize: function(args) {
    var self = this;
    _.bindAll(this, 'render', 'updateNewDaysToRemind');
    this.listenTo(this.model,'change:newdaystoremind', self.updateNewDaysToRemind);
    this.render();
  },
  destroy : function() {
    this.stopListening();
    $(this.el).remove();
  },
  updateNewDaysToRemind : function() {
     this.daysinput.setValue(this.model.newdaystoremind());
     this.calendar.setDays(this.model.newdaystoremind());
  },
  changeReminderDateBody : function() {
    var self = this;
    var model = self.model;
    var document = model.document();
    var div = $("<div class='autoreminder-modal-content'/>");

    var deadlinedecription = $("<div  class='line-before-calendar'/>").text(localization.autoreminders.dueDateIn +" " + document.timeouttime().diffDays() + " " + localization.autoreminders.days+" (" +document.timeouttime().toYMDString()+ ")");
    div.append(deadlinedecription);



    var label = $("<div class='text'/>");
    label.text(localization.autoreminders.daysToRemind + ':');
    var calendarbutton = $("<div class='calendarbutton'/>");
    self.calendar = new Calendar({on : calendarbutton,
                                         days : model.newdaystoremind(),
                                         maxValue : model.maxdays(),
                                         change: function(days) {
                                            if (days != model.newdaystoremind()) {
                                              model.setNewdaystoremind(days);
                                            }
                                          }
                        });

    self.daysinput = new InfoTextInput({
                infotext: "-",
                value: model.newdaystoremind(),
                onChange: function(v) {
                    days = parseInt(v);
                    if (isNaN(days))
                      days = undefined;
                    if (days != model.newdaystoremind()) {
                      model.setNewdaystoremind(days);
                    }
                    if (days == undefined && v != "") {
                      self.daysinput.setValue("");
                    } else if (days + "" != v && v != "") {
                      self.daysinput.setValue(days + "");
                    }
                }
            });

  div.append(label)
      .append(self.daysinput.el())
      .append($("<div class='text'/>").text(localization.designview.days))
      .append(calendarbutton);

  return div;
  },
  startSetReminderDateModal : function() {
      var self = this;
      self.modal = Confirmation.popup({
                title: localization.autoreminders.setAutoReminderTitle,
                subtitle : $("<div/>").html(localization.autoreminders.changeAutoreminderDescription),
                content: $(self.changeReminderDateBody()),
                width: 382,
                icon : '/img/modal-icons/extend-duedate.png',
                onReject : function() {
                  if (this.calendar != undefined)
                    this.calendar.close();
                },
                acceptButton : function() {
                  return self.buttonChangeForModal();
                }()
        });
  },
  buttonChangeForModal : function() {
    var self = this;
    return new Button({
      color: "green",
      style : (BrowserInfo.isSmallScreen() ? "margin-top:-10px" : ""),
      size: "small",
      text : (self.model.document().autoremindtime() != undefined ? localization.autoreminders.changeAutoreminderButton : localization.autoreminders.setAutoreminderButton),
      onClick : function() {
        if (self.model.newdaystoremind() == undefined) return; // This should never happend;
        self.model.setautoreminder(self.model.newdaystoremind(),function() {
          self.modal.close();
        });
      }
    }).el();
  },
  buttonClearForModal : function() {
    var self = this;
    return new Button({
      color: "black",
      style : "margin-right:10px;" + (BrowserInfo.isSmallScreen() ? "margin-top:-10px" : ""),
      size: "small",
      text : localization.autoreminders.removeAutoreminderButton,
      onClick : function() {
        self.model.setautoreminder(undefined,function() {
          self.modal.close();
        });
      }
    }).el();
  },
  startChangeReminderDateModal : function() {
      var self = this;
      self.modal = Confirmation.popup({
                title: localization.autoreminders.changeAutoReminderTitle,
                subtitle : $("<div/>").html(localization.autoreminders.changeAutoreminderDescription),
                content: $(self.changeReminderDateBody()),
                width: 382,
                icon : '/img/modal-icons/extend-duedate.png',
                onReject : function() {
                  if (this.calendar != undefined)
                    this.calendar.close();
                },
                acceptButton : function() {
                  var box = $("<div>")
                  return box.append(self.buttonClearForModal()).append(self.buttonChangeForModal());
                }()
        });
  },
  render: function() {
    var self = this;
    $(this.el).empty();

    var titlebox = $("<div class='titleinfo'>").append($("<div class='name'>").text(localization.autoreminders.automaticRemindersTitle));
    var contentbox = $("<div class='details'/>");
    if (self.model.document().autoremindtime() != undefined)
      contentbox.text(localization.autoreminders.willBeSentOn + ": " + self.model.document().autoremindtime().toYMDString());

    contentbox.append(new Button({
      color: "black",
      style: "margin-top: 10px",
      text : self.model.document().autoremindtime() == undefined ? localization.autoreminders.setDate : localization.autoreminders.changeDate ,
      size: "small",
      onClick : function() {
        if (self.model.document().autoremindtime() == undefined)
          self.startSetReminderDateModal();
        else
          self.startChangeReminderDateModal();
        return false;
      }
    }).el());

    $(this.el).append(titlebox).append($("<div class='inner'>").append(contentbox));

    return this;
  }

});

window.AuthorViewAutomaticReminders = function(args) {
          var model = new AuthorViewAutomaticRemindersModel(args);
          var view =  new AuthorViewAutomaticRemindersView({model : model, el :$("<div/>")});
          this.el = function() {return $(view.el);};
          this.destroy = function() { view.destroy()};

};


})(window);

