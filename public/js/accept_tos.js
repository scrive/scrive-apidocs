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

    termsPageContents: function() {
      var container = $('<div class="nicetext"/>');
      container.append($('<h2/>').append(localization.accountSetupModal.termsPageHeader));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage0));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage1Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1b));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1c));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1d));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1e));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage2Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2b));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2c));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage3Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3b));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3c));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3d));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage4Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage4));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage5Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage5));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage5a));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage6Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage6));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage7Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage7a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage7b));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage8Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage8));
      container.append($('<br/>'));
      return container;
    },

    render: function () {
      var model = this.model;
      var view = this;
      //var header = $("<header/>").append($("<h1 class='big'/>").text(localization.accountSetupModal.termsPageHeader));
      //$(this.el).append(header);
       
      var content = $("<div class='short-input-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");
      $(this.el).append(content.append(wrapper.append(body)));
      

      var terms = $('<div style="max-height: 400px; overflow: auto"/>');
      terms.append(this.termsPageContents());
      body.append(terms);

      var tosAccept = $("<div class='position'/>");
      var tosCBox = $("<input type='checkbox' id='tosCBox' name='tos' style='margin-right:10px;margin-top: -2px'/>");
      tosAccept.append(tosCBox);
      tosAccept.append($('<label for="tosCBox"/>').append(localization.accountSetupModal.modalAccountSetupBodyAcceptTOS));
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
    var view =  new AcceptTOSView({model: model, el: $("<div class='short-input-section accept-tos'/>")});
    this.el = function() {return $(view.el);}
  };

})(window);
