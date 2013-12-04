(function(window){

var AuthorViewAutomaticRemindersModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  authorview : function() {
     return this.get("authorview");
  },
  newdaystoremind: function() {
     return this.get("daystosign");
  },
  setNewdaystoremind: function(newdaystoremind) {
     var old = this.get("newdaystoremind")
     this.set({"newdaystoremind": newdaystoremind}, {silent: true});
     if (old != this.get("newdaystoremind"))
        this.trigger("change:newdaystoremind");
  },
  document :function() {
     return this.authorview().document();
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
    var doc = model.document();
    var labelText = "Days to remind";
    var div = $('<div />').addClass('design-view-action-process-left-column-deadline');

    var label = $('<div />');
    label.addClass('design-view-action-process-left-column-deadline-label');
    label.text("Days to remind" + ':');
    var calendarbutton = $("<div class='calendarbutton'/>");
    self.calendar = new Calendar({on : calendarbutton,
                                         days : model.newdaystoremind(),
                                         maxValue : "90",
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
  return div;



    return $("<div/>");
  },
  startSetReminderDatePopup : function() {
      var self = this;
      Confirmation.popup({
                title: "Set reminder date",
                content: $(self.changeReminderDateBody()),
                width: 382,
                icon : '/img/modal-icons/extend-duedate.png',
                acceptText: localization.prolongmodal.button,
                onAccept : function() {
                  model.prolong();
                  return true;
                }
        });
  },
  startChangeReminderDatePopup : function() {
      var self = this;
      Confirmation.popup({
                title: "Change reminder date",
                content: $(self.changeReminderDateBody()),
                width: 382,
                icon : '/img/modal-icons/extend-duedate.png',
                acceptText: localization.prolongmodal.button,
                onAccept : function() {
                  model.prolong();
                  return true;
                }
        });
  },
  render: function() {
    var self = this;
    $(this.el).empty();

    var titlebox = $("<div class='titleinfo spacing'>").append($("<div class='name'>").text("Automatic Reminders"));
    var contentbox = $("<div class='details'/>");
    if (self.model.document().autoremindtime() != undefined)
      contentbox.text("Will be sent at: " + self.model.document().autoremindtime().toYMDString());

    contentbox.append(new Button({
      color: "black",
      style: "margin-top: 10px",
      text : self.model.document().autoremindtime() == undefined ? "Set date" : "Change date",
      size: "small",
      onClick : function() {
        if (self.model.document().autoremindtime() == undefined)
          self.startSetReminderDatePopup();
        else
          self.startChangeReminderDatePopup();
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

