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
     if (1 > newdaystoremind || newdaystoremind > this.maxdays()) return;
     var old = this.get("newdaystoremind")
     this.set({"newdaystoremind": newdaystoremind}, {silent: true});
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
    this.model.bind('change:newdaystoremind', self.updateNewDaysToRemind);
    this.render();
  },
  destroy : function() {
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
    var container = $("<div style='text-align: center;'/>");

    var deadlinedecription = $("<div  style='text-align: center;'/>").text("Due date is in " + document.timeouttime().diffDays() + " day(s) (" +document.timeouttime().toYMDString()+ ")");
    container.append(deadlinedecription);

    var labelText = "Days to remind";
    var div = $("<div style='text-align: center;margin-top: 5px;'/>").addClass('design-view-action-process-left-column-deadline');


    var label = $("<div style='text-align: center;width:auto;'/>").addClass('design-view-action-process-left-column-deadline-label');
    label.text("Days to remind" + ':');
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
                cssClass : 'design-view-action-process-left-column-deadline-field',
                onChange: function(v) {
                    days = parseInt(v);
                    if (isNaN(days))
                      days = undefined;
                    if (days != model.newdaystoremind()) {
                      model.setNewdaystoremind(days);
                    } else if (days == undefined && v != "") {
                      self.daysinput.setValue("");
                    } else if (days + "" != v && v != "") {
                      self.daysinput.setValue(days + "");
                    }
                }
            });

  div.append(label)
      .append(self.daysinput.el())
      .append($("<div class='design-view-action-process-left-column-deadline-tag'/>").text(localization.designview.days))
      .append(calendarbutton);

  container.append(div);
  return container;
  },
  startSetReminderDateModal : function() {
      var self = this;
      self.modal = Confirmation.popup({
                title: "Set reminder date",
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
      style : "margin-left:10px;" + (BrowserInfo.isSmallScreen() ? "margin-top:-10px" : ""),
      size: "small",
      text : "Change",
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
      style : "margin-left:10px;" + (BrowserInfo.isSmallScreen() ? "margin-top:-10px" : ""),
      size: "small",
      text : "Clear",
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
                title: "Change reminder date",
                subtitle: $("<div/>Reminders will be sent to the next participant <BR/> who has not yet signed by this date</div>"),
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

    var titlebox = $("<div class='titleinfo spacing'>").append($("<div class='name'>").text("Automatic Reminders"));
    var contentbox = $("<div class='details'/>");
    if (self.model.document().autoremindtime() != undefined)
      contentbox.text("Will be sent on: " + self.model.document().autoremindtime().toYMDString());

    contentbox.append(new Button({
      color: "black",
      style: "margin-top: 10px",
      text : self.model.document().autoremindtime() == undefined ? "Set date" : "Change date",
      size: "small",
      onClick : function() {
        if (self.model.document().autoremindtime() == undefined)
          self.startSetReminderDateModal();
        else
          self.startChangeReminderDateModal();
        return false;
      }
    }).el());

    $(this.el).append(titlebox).append($("<div class='inner spacing'>").append(contentbox));

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

