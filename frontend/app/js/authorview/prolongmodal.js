/* This modal asks user for how long he wants to extend document timeout time.
 * Usage:
 * new ProlongModal({authorview : authorview});
 */

define(['Backbone', 'legacy_code'], function() {

var ProlongModalModel = Backbone.Model.extend({
  defaults : {
      days : "1"
  },
  initialize : function(){
  },
  days : function() {
    return this.get("days");
  },
  setDays: function(days) {
    this.set({"days": days});
  },
  authorview : function() {
    return  this.get("authorview");
  },
  document : function() {
    return this.authorview().document();
  },
  prolong : function() {
    var self = this;
    LoadingDialog.open();
    this.document().prolong(self.days()).sendAjax(function() {
      self.authorview().reload(true);
    });
  }
});

var ProlongModalView = Backbone.View.extend({
    initialize: function(args) {
        var self = this;
        _.bindAll(this, 'render', 'updateDays');
        this.listenTo(this.model,'change', self.updateDays);
        this.render();
    },
    destroy: function () {
      this.stopListening();
      $(this.el).remove();
    },
    updateDays: function () {
      this.daysinputfield.setValue(this.model.days());
      this.calendar.setDays(this.model.days());
    },
    render: function() {
      var self = this;
      var model = this.model;
      var container = $(this.el);

      var calendarbutton = $("<div class='calendarbutton'/>");
      self.calendar = new Calendar({on: calendarbutton,
                                    days: model.days(),
                                    change: function(days) {
                                      if (days != model.days()) {
                                        model.setDays(days);
                                      }
                                    }
                                   });

      self.daysinputfield = new InfoTextInput({
                infotext: model.days(),
                value: model.days(),
                onChange: function(v) {
                    var days = parseInt(v);
                    if (isNaN(days)) {
                      days = undefined;
                    } else {
                      days = Math.min(90, days);
                      days = Math.max(1, days);
                    }
                    if (days != model.days()) {
                      model.setDays(days);
                    } else if (days == undefined && v != "") {
                      self.daysinputfield.setValue("");
                    } else if (days + "" != v && v != "") {
                      self.daysinputfield.setValue(days + "");
                    }
                }
            });

      container.append($("<div class='text'/>").text(localization.prolongmodal.text))
               .append(self.daysinputfield.el())
               .append($("<div class='text'/>").text(localization.prolongmodal.days))
               .append(calendarbutton);
      return this;
    }
});


window.ProlongModal = function(args){
        var model = new ProlongModalModel( {
                        authorview : args.authorview
                    });
        var view = new ProlongModalView ({
                        model: model,
                        el: $("<div class='prolong-modal-content'/>")
                    });
        new Confirmation({
                title: localization.prolongmodal.title,
                content: $(view.el),
                width: 382,
                acceptText: localization.prolongmodal.button,
                onAccept : function() {
                  model.prolong();
                  return true;
                }
        });
};

});
