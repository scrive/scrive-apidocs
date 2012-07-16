/* All things that are shown after person had signed */


(function(window) {

window.DocumentShareAfterSignView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  createFacebookLikeBox: function() {
    return $('<iframe src="//www.facebook.com/plugins/likebox.php?href=http%3A%2F%2Fwww.facebook.com%2Fpages%2FScrive%2F237391196280189&amp;width=292&amp;height=62&amp;colorscheme=light&amp;show_faces=false&amp;border_color&amp;stream=false&amp;header=false" scrolling="no" frameborder="0" style="border:none; overflow:hidden; width:292px; height:62px;" allowTransparency="true"></iframe>');
  },
  createFacebookLikeElems: function() {
    var container = $("<div />");

    var button = $("<div class='facebook btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.facebookButtonLabel));
    container.append(button);

    var dropdown = $("<div class='facebook dropdown'/>");
    dropdown.append($("<div class='content' />").append(this.createFacebookLikeBox()));
    dropdown.hide();
    container.append(dropdown);

    button.mouseover(function() {
      dropdown.addClass("over");
    });

    button.mouseleave(function() {
      dropdown.removeClass("over");
    });

    button.click(function() {
      dropdown.toggle();
    });
    return container;
  },
  createTweetLink: function() {
    var container = $("<a />");
    container.attr("href", "https://twitter.com/intent/tweet?screen_name=scrive");
    var twitterscript = $("<script type='text/javascript' />");
    twitterscript.attr("src", "//platform.twitter.com/widgets.js");
    console.log("adding twitter script to head");
    $("head").append(twitterscript);
    return container;
  },
  createTweetThisElems: function() {
    var container = this.createTweetLink();
    var button = $("<div class='twitter btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.tweetButtonLabel));
    container.append(button);
    return container;
  },
  createPhoneMeElems: function() {
    var model = this.model;

    var container = $("<div />");

    var button = $("<div class='phone btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.phoneButtonLabel));
    container.append(button);

    var loading = $("<div class='loading' />");
    loading.hide();

    var form = $("<div />");
    form.append($("<div />").text(localization.docsignview.phoneFormDescription));
    var numberinput = $("<input type='text' />");
    form.append(numberinput);
    var submitForm = function() {
      var phone = numberinput.val();
      if (phone.trim().length == 0) {
        return;
      }
      (new Submit({
        url: model.phoneurl,
        method: "POST",
        email: model.email(),
        phone: phone,
        ajax: true,
        onSend: function() {
          console.log("requesting phone call");
          form.hide();
          loading.show();
        },
        ajaxerror: function(d, a) {
          console.error("failed to request a phone call");
          loading.hide();
          form.show();
        },
        ajaxsuccess: function(d) {
          console.log("successfully requested a phone call");
          loading.hide();
          form.empty();
          form.text(localization.docsignview.phoneConfirmationText);
          form.show();
        }
      })).send();
    };

    form.append(Button.init({
      color: "green",
      size: "tiny",
      text: localization.docsignview.phoneSubmitButtonLabel,
      onClick: submitForm
    }).input());

    numberinput.keypress(function(e) {
      if (e.which == 13) {
        submitForm();
      }
    });

    form.append($("<div class='clearfix' />"));

    var content = $("<div class='content' />");
    content.append(loading);
    content.append(form);

    var dropdown = $("<div class='phone dropdown'/>");
    dropdown.append(content);
    dropdown.hide();
    container.append(dropdown);

    button.mouseover(function() {
      dropdown.addClass("over");
    });

    button.mouseleave(function() {
      dropdown.removeClass("over");
    });

    button.click(function() {
      dropdown.toggle();
    });
    return container;
  },
  createStartLink: function() {
    var container = $("<a />");
    container.attr("href", "/upload");
    return container;
  },
  createGetStartedElems: function() {
    var container = this.createStartLink();
    var button = $("<div class='start btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.startButtonLabel));
    container.append(button);
    return container;
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.hasSigned() || !this.model.saved() || this.model.document().isWhiteLabeled() || this.model.document().currentSignatory().hasUser()) {
      return this;
    }

    var container = $("<div class='share' />");

    container.append($("<div class='title'/>").text(localization.docsignview.shareTitle));
    var panel = $("<div class='panel' />");
    panel.append($("<div class='item' />").append(this.createFacebookLikeElems()));
    panel.append($("<div class='item' />").append(this.createTweetThisElems()));
    panel.append($("<div class='item' />").append(this.createPhoneMeElems()));
    panel.append($("<div class='item' />").append(this.createGetStartedElems()));
    panel.append($("<div class='clearfix' />"));
    container.append(panel);

    $(this.el).append(container);
    return this;
  }
});
})(window);
