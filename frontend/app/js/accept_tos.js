define(['Backbone', 'legacy_code'], function() {

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
      var tosCBox = $("<div class='checkbox' name='tos' style='margin-left:3px'/>");
      if (model.accepted()) {
        tosCBox.addClass('checked');
      }
      var toggleCheckBox = function() {
        tosCBox.toggleClass('checked');
        model.setAccepted(tosCBox.hasClass('checked'));
      };
      tosCBox.click(toggleCheckBox);

      tosAccept.append(tosCBox);

      var tosLabel = $("<span/>").append($(localization.accountSetupModal.modalAccountSetupTOS));
      tosLabel.find('label').click(toggleCheckBox);
      var tosA = tosLabel.find('.is-TOS')
                .attr("class", "clickable")
                .attr("target", "_blank")
                .attr("href", "/terms");
      tosA.text(" " + tosA.text());
      tosAccept.append(tosLabel);
      body.append(tosAccept);

      var acceptButton = new Button({
          size: 'small',
          color: 'green',
          text: localization.accept,
          onClick: function() {
            if (!model.accept())
                 tosAccept.css("border","1px solid red");
          }
        });

      body.append($("<div class='position' style='text-align:center'/>").append(acceptButton.el()));

    }
  });

  window.AcceptTOS = function(args) {
    var model = new AcceptTOSModel(args);
    var view =  new AcceptTOSView({model: model, el: $("<div class='short-input-section accept-tos s-accept-tos'/>")});
    this.el = function() {return $(view.el);};
  };

});
