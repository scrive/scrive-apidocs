(function(window) {

  var AcceptTOSModel = Backbone.Model.extend({
    defaults: {
      accepted: false
    },
    accepted: function() {
      return this.get('accepted');
    },
    setAccepted: function(accepted) {
      this.set('accepted', accepted);
    },
    accept: function() {
      var model = this;

      if (!model.accepted()) {
        return false;
      }

      new Submit({
        method: 'POST',
        ajax: true,
        tos: 'on',
        ajaxsuccess: function(rs) {
            window.location = "/newdocument";
        }
      }).send();
      return true;
    }
  });

  var AcceptTOSView = Backbone.View.extend({
    initialize: function() {
      this.render();
      _.bindAll(this, 'render');
    },
    render: function () {
      var model = this.model;
      var view = this;

      var content = $("<div class='short-input-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");
      $(this.el).append(content.append(wrapper.append(body)));


      var tosAccept = $("<div class='position first'/>");
      var tosCBox = $("<input type='checkbox' id='tosCBox' name='tos' class='s-accept-tos-cbox' style='margin-right:10px;margin-top: -2px'/>");
      tosAccept.append(tosCBox);
      var thref = "http://" + location.host + location.pathname.substring(0, 3) + "/terms";
      tosAccept.append($('<span/>')
                  .append($("<label/>").text(localization.accountSetupModal.modalAccountSetupBodyAccept))
                  .append($("<a class='clickable' target='_blank'/>").attr('href',thref).text(" " + localization.accountSetupModal.modalAccountSetupBodyTOS))
                );
      tosAccept.append($('<br/>'));
      body.append(tosAccept);
      tosCBox.change(function() {
        if (tosCBox.attr('checked')) {
          model.setAccepted(true);
          tosAccept.css("border-width","0px");
        } else {
          model.setAccepted(false);
          tosAccept.css("border-width","0px");
        }
      });
      var acceptButton = Button.init({
          size: 'small',
          color: 'green',
          text: localization.send,
          onClick: function() {
            if (!model.accept())
                 tosAccept.css("border","1px solid red");
          }
        });

      body.append($("<div class='position' style='text-align:center'/>").append(acceptButton.input()));

    }
  });

  window.AcceptTOS = function(args) {
    var model = new AcceptTOSModel(args);
    var view =  new AcceptTOSView({model: model, el: $("<div class='short-input-section accept-tos s-accept-tos'/>")});
    this.el = function() {return $(view.el);};
  };

})(window);
